import assert from 'node:assert/strict';
import {mkdtemp, mkdir, rm, writeFile} from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import test from 'node:test';
import {scanRuns} from './report-data.js';

function validNetwork(overrides = {}) {
  return {
    send_octets: 4096,
    average_packet_bytes: 512,
    ...overrides
  };
}

function validSchedulers(overrides = {}) {
  return {
    utilization_percent: 72.5,
    maximum_run_queue_length: 8,
    ...overrides
  };
}

function validMetrics(overrides = {}) {
  return {
    memory: {maximum_bytes: 1200},
    network: validNetwork(),
    schedulers: validSchedulers(),
    ...overrides
  };
}

function validPoint(overrides = {}) {
  return {
    schema_version: 7,
    operation: 'send',
    path: 'native',
    payload: 'tiny',
    writer_count: 10,
    messages_per_writer: 100,
    pace_ms: 10,
    distribution_busy_limit_kib: 1024,
    sender_config: 'local',
    receiver_config: 'runner@receiver.example.net',
    elapsed_ms: 1000,
    metrics: validMetrics(),
    ...overrides
  };
}

function schemaTwoPoint(overrides = {}) {
  const metrics = validMetrics();
  delete metrics.schedulers;
  metrics.locks = {wait_us: 50, collision_percent: 1.5};
  return validPoint({schema_version: 2, metrics, ...overrides});
}

function legacyPoint(overrides = {}) {
  return {
    schema_version: 1,
    operation: 'send',
    path: 'native',
    payload: 'tiny',
    writer_count: 10,
    messages_per_writer: 100,
    pace_ms: 10,
    distribution_busy_limit_kib: 1024,
    sender_config: 'local',
    receiver_config: 'runner@receiver.example.net',
    elapsed_ms: 1000,
    performance_percent: 100,
    metrics: {
      memory: {average_bytes: 1000, maximum_bytes: 1200},
      locks: {
        wait_us: 50,
        collision_percent: 1.5,
        duration_percent: 0.5
      }
    },
    ...overrides
  };
}

test('scans runs and reports malformed point files', async () => {
  const temporaryRoot = await mkdtemp(
    path.join(os.tmpdir(), 'ecall-performance-report-'));
  try {
    const dataDirectory = path.join(
      temporaryRoot,
      'ct_run.second',
      'performance_send_SUITE.logs',
      'log_private',
      'performance_data');
    await mkdir(dataDirectory, {recursive: true});
    await writeFile(
      path.join(dataDirectory, 'send.native.tiny.10.json'),
      JSON.stringify(validPoint()));
    await writeFile(
      path.join(dataDirectory, 'send.ecall.tiny.10.json'),
      '{unfinished');
    await mkdir(path.join(temporaryRoot, 'ct_run.first'));

    const result = await scanRuns(temporaryRoot);

    assert.deepEqual(result.runs.map(({id}) => id), [
      'ct_run.second',
      'ct_run.first'
    ]);
    assert.equal(result.runs[0].points.length, 1);
    assert.equal(result.runs[0].errors.length, 1);
    assert.match(result.runs[0].errors[0].file, /send\.ecall\.tiny\.10\.json$/);
    assert.equal(result.runs[1].points.length, 0);
  } finally {
    await rm(temporaryRoot, {recursive: true, force: true});
  }
});

test('skips JSON that does not match the point schema', async () => {
  const temporaryRoot = await mkdtemp(
    path.join(os.tmpdir(), 'ecall-performance-report-schema-'));
  try {
    const dataDirectory = path.join(
      temporaryRoot,
      'ct_run.invalid',
      'performance_send_SUITE.logs',
      'log_private',
      'performance_data');
    await mkdir(dataDirectory, {recursive: true});
    await writeFile(
      path.join(dataDirectory, 'empty.json'),
      JSON.stringify({}));
    await writeFile(
      path.join(dataDirectory, 'invalid-path.json'),
      JSON.stringify(validPoint({path: 'other'})));
    await writeFile(
      path.join(dataDirectory, 'invalid-metric.json'),
      JSON.stringify(validPoint({
        metrics: validMetrics({memory: {maximum_bytes: '1200'}})
      })));
    await writeFile(
      path.join(dataDirectory, 'invalid-sender-config.json'),
      JSON.stringify(validPoint({sender_config: ''})));
    await writeFile(
      path.join(dataDirectory, 'invalid-receiver-config.json'),
      JSON.stringify(validPoint({receiver_config: {host: 'receiver'}})));

    const result = await scanRuns(temporaryRoot);

    assert.equal(result.runs[0].points.length, 0);
    assert.equal(result.runs[0].errors.length, 5);
    assert.ok(result.runs[0].errors.every(
      error => error.message.startsWith('invalid performance point field:')));
    assert.ok(result.runs[0].errors.some(
      error => error.message.endsWith('schema_version')));
    assert.ok(result.runs[0].errors.some(
      error => error.message.endsWith('path')));
    assert.ok(result.runs[0].errors.some(
      error => error.message.endsWith('metrics.memory.maximum_bytes')));
    assert.ok(result.runs[0].errors.some(
      error => error.message.endsWith('sender_config')));
    assert.ok(result.runs[0].errors.some(
      error => error.message.endsWith('receiver_config')));
  } finally {
    await rm(temporaryRoot, {recursive: true, force: true});
  }
});

test('rejects schema-version-2 points with invalid network metrics', async () => {
  const temporaryRoot = await mkdtemp(
    path.join(os.tmpdir(), 'ecall-performance-report-network-'));
  try {
    const dataDirectory = path.join(
      temporaryRoot,
      'ct_run.network',
      'performance_send_SUITE.logs',
      'log_private',
      'performance_data');
    await mkdir(dataDirectory, {recursive: true});
    const withoutNetwork = validMetrics();
    delete withoutNetwork.network;
    delete withoutNetwork.schedulers;
    await writeFile(
      path.join(dataDirectory, 'missing-network.json'),
      JSON.stringify(schemaTwoPoint({metrics: withoutNetwork})));
    await writeFile(
      path.join(dataDirectory, 'bad-octets.json'),
      JSON.stringify(schemaTwoPoint({
        metrics: validMetrics({network: validNetwork({send_octets: -1})})
      })));
    await writeFile(
      path.join(dataDirectory, 'bad-packet.json'),
      JSON.stringify(schemaTwoPoint({
        metrics: validMetrics({
          network: validNetwork({average_packet_bytes: '512'})
        })
      })));

    const result = await scanRuns(temporaryRoot);

    assert.equal(result.runs[0].points.length, 0);
    assert.equal(result.runs[0].errors.length, 3);
    assert.ok(result.runs[0].errors.some(
      error => error.message.endsWith('metrics.network')));
    assert.ok(result.runs[0].errors.some(
      error => error.message.endsWith('metrics.network.send_octets')));
    assert.ok(result.runs[0].errors.some(
      error => error.message.endsWith('metrics.network.average_packet_bytes')));
  } finally {
    await rm(temporaryRoot, {recursive: true, force: true});
  }
});

test('rejects schema-version-7 points with invalid scheduler metrics', async () => {
  const temporaryRoot = await mkdtemp(
    path.join(os.tmpdir(), 'ecall-performance-report-schedulers-'));
  try {
    const dataDirectory = path.join(
      temporaryRoot,
      'ct_run.schedulers',
      'performance_send_SUITE.logs',
      'log_private',
      'performance_data');
    await mkdir(dataDirectory, {recursive: true});
    const withoutSchedulers = validMetrics();
    delete withoutSchedulers.schedulers;
    await writeFile(
      path.join(dataDirectory, 'missing-schedulers.json'),
      JSON.stringify(validPoint({metrics: withoutSchedulers})));
    await writeFile(
      path.join(dataDirectory, 'bad-utilization.json'),
      JSON.stringify(validPoint({
        metrics: validMetrics({
          schedulers: validSchedulers({utilization_percent: -1})
        })
      })));
    await writeFile(
      path.join(dataDirectory, 'bad-utilization-type.json'),
      JSON.stringify(validPoint({
        metrics: validMetrics({
          schedulers: validSchedulers({utilization_percent: '72.5'})
        })
      })));
    await writeFile(
      path.join(dataDirectory, 'bad-run-queue.json'),
      JSON.stringify(validPoint({
        metrics: validMetrics({
          schedulers: validSchedulers({maximum_run_queue_length: 8.5})
        })
      })));

    const result = await scanRuns(temporaryRoot);

    assert.equal(result.runs[0].points.length, 0);
    assert.equal(result.runs[0].errors.length, 4);
    assert.ok(result.runs[0].errors.some(
      error => error.message.endsWith('metrics.schedulers')));
    assert.equal(
      result.runs[0].errors.filter(
        error =>
          error.message.endsWith('metrics.schedulers.utilization_percent')
      ).length,
      2);
    assert.ok(result.runs[0].errors.some(
      error =>
        error.message.endsWith('metrics.schedulers.maximum_run_queue_length')));
  } finally {
    await rm(temporaryRoot, {recursive: true, force: true});
  }
});

test('accepts schema-version-2 points without scheduler metrics', async () => {
  const temporaryRoot = await mkdtemp(
    path.join(os.tmpdir(), 'ecall-performance-report-legacy-v2-'));
  try {
    const dataDirectory = path.join(
      temporaryRoot,
      'ct_run.legacy_v2',
      'performance_send_SUITE.logs',
      'log_private',
      'performance_data');
    await mkdir(dataDirectory, {recursive: true});
    await writeFile(
      path.join(dataDirectory, 'legacy.json'),
      JSON.stringify(schemaTwoPoint()));

    const result = await scanRuns(temporaryRoot);

    assert.equal(result.runs[0].points.length, 1);
    assert.equal(result.runs[0].errors.length, 0);
    assert.equal(result.runs[0].points[0].schema_version, 2);
    assert.equal(result.runs[0].points[0].metrics.schedulers, undefined);
  } finally {
    await rm(temporaryRoot, {recursive: true, force: true});
  }
});

test('accepts legacy schema-version-1 points without network metrics', async () => {
  const temporaryRoot = await mkdtemp(
    path.join(os.tmpdir(), 'ecall-performance-report-legacy-v1-'));
  try {
    const dataDirectory = path.join(
      temporaryRoot,
      'ct_run.legacy_v1',
      'performance_send_SUITE.logs',
      'log_private',
      'performance_data');
    await mkdir(dataDirectory, {recursive: true});
    await writeFile(
      path.join(dataDirectory, 'legacy.json'),
      JSON.stringify(legacyPoint()));

    const result = await scanRuns(temporaryRoot);

    assert.equal(result.runs[0].points.length, 1);
    assert.equal(result.runs[0].errors.length, 0);
    assert.equal(result.runs[0].points[0].schema_version, 1);
  } finally {
    await rm(temporaryRoot, {recursive: true, force: true});
  }
});

test('accepts legacy points without role configuration', async () => {
  const temporaryRoot = await mkdtemp(
    path.join(os.tmpdir(), 'ecall-performance-report-legacy-'));
  try {
    const dataDirectory = path.join(
      temporaryRoot,
      'ct_run.legacy',
      'performance_send_SUITE.logs',
      'log_private',
      'performance_data');
    await mkdir(dataDirectory, {recursive: true});
    const point = validPoint();
    delete point.sender_config;
    delete point.receiver_config;
    await writeFile(
      path.join(dataDirectory, 'legacy.json'),
      JSON.stringify(point));

    const result = await scanRuns(temporaryRoot);

    assert.equal(result.runs[0].points.length, 1);
    assert.equal(result.runs[0].errors.length, 0);
    assert.equal(result.runs[0].points[0].sender_config, undefined);
    assert.equal(result.runs[0].points[0].receiver_config, undefined);
  } finally {
    await rm(temporaryRoot, {recursive: true, force: true});
  }
});

test('returns no runs when the Common Test log root does not exist', async () => {
  const result = await scanRuns('/no/such/ecall/performance/logs');
  assert.deepEqual(result.runs, []);
});
