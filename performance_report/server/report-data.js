import {readdir, readFile, stat} from 'node:fs/promises';
import path from 'node:path';

const operations = new Set(['send', 'cast', 'call']);
const paths = new Set(['native', 'ecall']);
const schemaVersions = new Set([1, 2, 3, 4, 5]);

function requireValue(condition, field) {
  if (!condition) {
    throw new Error(`invalid performance point field: ${field}`);
  }
}

function isFiniteNumber(value) {
  return typeof value === 'number' && Number.isFinite(value);
}

function isPositiveInteger(value) {
  return Number.isInteger(value) && value > 0;
}

function validateOptionalString(point, field) {
  const value = point[field];
  requireValue(
    value === undefined ||
      (typeof value === 'string' && value.length > 0),
    field);
}

function validatePoint(point) {
  requireValue(
    point !== null && typeof point === 'object' && !Array.isArray(point),
    'root');
  requireValue(schemaVersions.has(point.schema_version), 'schema_version');
  requireValue(operations.has(point.operation), 'operation');
  requireValue(paths.has(point.path), 'path');
  requireValue(
    typeof point.payload === 'string' && point.payload.length > 0,
    'payload');
  requireValue(isPositiveInteger(point.writer_count), 'writer_count');
  requireValue(
    isPositiveInteger(point.messages_per_writer),
    'messages_per_writer');
  requireValue(isFiniteNumber(point.pace_ms) && point.pace_ms > 0, 'pace_ms');
  requireValue(
    isPositiveInteger(point.distribution_busy_limit_kib),
    'distribution_busy_limit_kib');
  validateOptionalString(point, 'sender_config');
  validateOptionalString(point, 'receiver_config');
  requireValue(
    isFiniteNumber(point.elapsed_ms) && point.elapsed_ms >= 0,
    'elapsed_ms');
  requireValue(
    isFiniteNumber(point.performance_percent) &&
      point.performance_percent >= 0,
    'performance_percent');

  const memory = point.metrics?.memory;
  requireValue(
    isFiniteNumber(memory?.maximum_bytes) && memory.maximum_bytes >= 0,
    'metrics.memory.maximum_bytes');

  const locks = point.metrics?.locks;
  requireValue(
    isFiniteNumber(locks?.wait_us) && locks.wait_us >= 0,
    'metrics.locks.wait_us');
  requireValue(
    isFiniteNumber(locks?.collision_percent) &&
      locks.collision_percent >= 0,
    'metrics.locks.collision_percent');

  if (point.schema_version >= 2) {
    validateNetwork(point.metrics?.network);
  }
  if (point.schema_version >= 4) {
    validateLoad(point.metrics?.load);
  }
  return point;
}

function validateNonNegativeNumber(value, field) {
  requireValue(isFiniteNumber(value) && value >= 0, field);
}

function validateNetwork(network) {
  requireValue(
    network !== null && typeof network === 'object' &&
      !Array.isArray(network),
    'metrics.network');
  validateNonNegativeNumber(network.send_octets, 'metrics.network.send_octets');
  validateNonNegativeNumber(
    network.average_packet_bytes,
    'metrics.network.average_packet_bytes');
  validateNonNegativeNumber(
    network.send_pending?.maximum_bytes,
    'metrics.network.send_pending.maximum_bytes');
  validateNonNegativeNumber(
    network.busy_dist_port_events,
    'metrics.network.busy_dist_port_events');
}

function validateLoad(load) {
  requireValue(
    load !== null && typeof load === 'object' && !Array.isArray(load),
    'metrics.load');
  validateNonNegativeNumber(load.average_1m, 'metrics.load.average_1m');
}

async function pointFiles(directory) {
  const entries = await readdir(directory, {withFileTypes: true});
  const files = await Promise.all(entries.map(async (entry) => {
    const entryPath = path.join(directory, entry.name);
    if (entry.isDirectory()) {
      return pointFiles(entryPath);
    }
    if (entry.isFile() && entry.name.endsWith('.json') &&
        path.basename(directory) === 'performance_data' &&
        directory.split(path.sep).includes('log_private')) {
      return [entryPath];
    }
    return [];
  }));
  return files.flat();
}

async function parsePoint(file, runDirectory) {
  const relativeFile = path.relative(runDirectory, file);
  try {
    const point = JSON.parse(await readFile(file, 'utf8'));
    return {
      point: validatePoint(point),
      error: null
    };
  } catch (error) {
    return {
      point: null,
      error: {file: relativeFile, message: error.message}
    };
  }
}

async function scanRun(logsRoot, entry) {
  const runDirectory = path.join(logsRoot, entry.name);
  const files = await pointFiles(runDirectory);
  const parsed = await Promise.all(
    files.map((file) => parsePoint(file, runDirectory)));
  const info = await stat(runDirectory);
  return {
    id: entry.name,
    modified_at: info.mtime.toISOString(),
    report_url: `/ct-logs/${encodeURIComponent(entry.name)}/index.html`,
    points: parsed.flatMap(({point}) => point === null ? [] : [point]),
    errors: parsed.flatMap(({error}) => error === null ? [] : [error])
  };
}

export async function scanRuns(logsRoot) {
  let entries;
  try {
    entries = await readdir(logsRoot, {withFileTypes: true});
  } catch (error) {
    if (error.code === 'ENOENT') {
      return {logs_root: logsRoot, runs: []};
    }
    throw error;
  }

  const runEntries = entries.filter(
    (entry) => entry.isDirectory() && entry.name.startsWith('ct_run.'));
  const runs = await Promise.all(
    runEntries.map((entry) => scanRun(logsRoot, entry)));
  runs.sort((left, right) => right.id.localeCompare(left.id));
  return {logs_root: logsRoot, runs};
}
