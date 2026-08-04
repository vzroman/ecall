import {useEffect, useMemo, useState} from 'react';
import {
  CategoryScale,
  Chart as ChartJS,
  Filler,
  Legend,
  LinearScale,
  LineElement,
  PointElement,
  Tooltip
} from 'chart.js';
import {Line} from 'react-chartjs-2';

ChartJS.register(
  CategoryScale,
  Filler,
  Legend,
  LinearScale,
  LineElement,
  PointElement,
  Tooltip
);

const REFRESH_INTERVAL_MS = 5000;
const paths = ['native', 'ecall'];
const metrics = [
  {id: 'performance_percent', label: 'Performance', unit: '%', value: point => point.performance_percent},
  {id: 'average_memory', label: 'Average memory', unit: 'bytes', value: point => point.metrics?.memory?.average_bytes},
  {id: 'maximum_memory', label: 'Maximum memory', unit: 'bytes', value: point => point.metrics?.memory?.maximum_bytes},
  {id: 'lock_wait', label: 'Lock wait', unit: 'µs', value: point => point.metrics?.locks?.wait_us},
  {id: 'lock_collision', label: 'Lock collisions', unit: '%', value: point => point.metrics?.locks?.collision_percent},
  {id: 'lock_duration', label: 'Lock duration', unit: '%', value: point => point.metrics?.locks?.duration_percent}
];

function configKey(point) {
  return [
    point.operation,
    point.payload,
    point.messages_per_writer,
    point.pace_ms,
    point.distribution_busy_limit_kib
  ].join('|');
}

function groupPoints(points) {
  const groups = new Map();
  points.forEach((point) => {
    const key = configKey(point);
    if (!groups.has(key)) {
      groups.set(key, {
        operation: point.operation,
        payload: point.payload,
        messagesPerWriter: point.messages_per_writer,
        paceMs: point.pace_ms,
        busyLimitKiB: point.distribution_busy_limit_kib,
        points: []
      });
    }
    groups.get(key).points.push(point);
  });
  return [...groups.values()].sort((left, right) =>
    `${left.operation}.${left.payload}`.localeCompare(
      `${right.operation}.${right.payload}`));
}

function valueAt(points, path, writerCount, metric) {
  const point = points.find(candidate =>
    candidate.path === path && candidate.writer_count === writerCount);
  const value = point === undefined ? undefined : metric.value(point);
  return Number.isFinite(value) ? value : null;
}

function formatValue(value, unit) {
  if (value === null) return 'N/A';
  if (unit === 'bytes') {
    return new Intl.NumberFormat().format(Math.round(value));
  }
  return `${new Intl.NumberFormat(undefined, {maximumFractionDigits: 2}).format(value)} ${unit}`;
}

function MetricGrid({group, writerCounts}) {
  return (
    <div className="table-scroll">
      <table>
        <thead>
          <tr>
            <th>Path / metric</th>
            {writerCounts.map(count => <th key={count}>{count.toLocaleString()}</th>)}
          </tr>
        </thead>
        <tbody>
          {paths.flatMap(path => metrics.map(metric => (
            <tr key={`${path}.${metric.id}`}>
              <th><span className={`path-marker ${path}`} />{path} · {metric.label}</th>
              {writerCounts.map(count => (
                <td key={count}>{formatValue(valueAt(group.points, path, count, metric), metric.unit)}</td>
              ))}
            </tr>
          )))}
        </tbody>
      </table>
    </div>
  );
}

function MetricChart({group, metric, writerCounts}) {
  const data = {
    datasets: paths.map(path => ({
      label: path,
      data: writerCounts.map(writerCount => ({
        x: writerCount,
        y: valueAt(group.points, path, writerCount, metric)
      })),
      borderColor: path === 'native' ? '#2563eb' : '#e11d48',
      backgroundColor: path === 'native' ? '#2563eb' : '#e11d48',
      tension: 0.2,
      spanGaps: false
    }))
  };
  const options = {
    animation: false,
    maintainAspectRatio: false,
    parsing: false,
    plugins: {legend: {position: 'bottom'}},
    scales: {
      x: {type: 'linear', title: {display: true, text: 'Writer count'}},
      y: {title: {display: true, text: metric.unit}, beginAtZero: false}
    }
  };
  return (
    <article className="chart-card">
      <h4>{metric.label}</h4>
      <div className="chart"><Line data={data} options={options} /></div>
    </article>
  );
}

function PointGroup({group}) {
  const writerCounts = [...new Set(group.points.map(point => point.writer_count))]
    .sort((left, right) => left - right);
  return (
    <section className="point-group">
      <header>
        <div>
          <span className="eyebrow">{group.operation}</span>
          <h2>{group.payload} payload</h2>
        </div>
        <dl className="config">
          <div><dt>Messages / writer</dt><dd>{group.messagesPerWriter}</dd></div>
          <div><dt>Pace</dt><dd>{group.paceMs} ms</dd></div>
          <div><dt>Busy limit</dt><dd>{group.busyLimitKiB} KiB</dd></div>
        </dl>
      </header>
      <MetricGrid group={group} writerCounts={writerCounts} />
      <div className="charts">
        {metrics.map(metric => (
          <MetricChart
            key={metric.id}
            group={group}
            metric={metric}
            writerCounts={writerCounts}
          />
        ))}
      </div>
    </section>
  );
}

function EmptyState() {
  return (
    <div className="empty">
      <h2>No performance runs found</h2>
      <p>Run the Common Test performance suite; this page refreshes automatically.</p>
    </div>
  );
}

export default function App() {
  const [report, setReport] = useState({runs: []});
  const [selectedRunId, setSelectedRunId] = useState(null);
  const [loadError, setLoadError] = useState(null);
  const [updatedAt, setUpdatedAt] = useState(null);

  async function refresh() {
    try {
      const response = await fetch('/api/report');
      if (!response.ok) throw new Error(`HTTP ${response.status}`);
      const nextReport = await response.json();
      setReport(nextReport);
      setSelectedRunId(current =>
        nextReport.runs.some(run => run.id === current)
          ? current
          : nextReport.runs[0]?.id ?? null);
      setLoadError(null);
      setUpdatedAt(new Date());
    } catch (error) {
      setLoadError(error.message);
    }
  }

  useEffect(() => {
    refresh();
    const timer = window.setInterval(refresh, REFRESH_INTERVAL_MS);
    return () => window.clearInterval(timer);
  }, []);

  const selectedRun = report.runs.find(run => run.id === selectedRunId);
  const groups = useMemo(
    () => groupPoints(selectedRun?.points ?? []),
    [selectedRun]);

  return (
    <main>
      <header className="page-header">
        <div>
          <span className="eyebrow">Common Test metrics</span>
          <h1>ecall performance</h1>
          <p>Native Erlang distribution and ecall, compared at each writer count.</p>
        </div>
        <div className="toolbar">
          <label>
            Run
            <select
              value={selectedRunId ?? ''}
              onChange={event => setSelectedRunId(event.target.value)}
              disabled={report.runs.length === 0}
            >
              {report.runs.map(run => (
                <option key={run.id} value={run.id}>{run.id}</option>
              ))}
            </select>
          </label>
          <button type="button" onClick={refresh}>Refresh</button>
        </div>
      </header>

      <div className="status">
        <span>{updatedAt ? `Updated ${updatedAt.toLocaleTimeString()}` : 'Loading…'}</span>
        {selectedRun && <a href={selectedRun.report_url}>Open Common Test report ↗</a>}
      </div>

      {loadError && <div className="alert">Could not load report data: {loadError}</div>}
      {selectedRun?.errors.length > 0 && (
        <details className="alert" open>
          <summary>{selectedRun.errors.length} point file(s) could not be parsed</summary>
          <ul>
            {selectedRun.errors.map(error => (
              <li key={error.file}><code>{error.file}</code>: {error.message}</li>
            ))}
          </ul>
        </details>
      )}

      {report.runs.length === 0
        ? <EmptyState />
        : groups.length === 0
          ? <div className="empty"><h2>No completed points in this run</h2></div>
          : groups.map(group => <PointGroup key={configKey(group.points[0])} group={group} />)}
    </main>
  );
}
