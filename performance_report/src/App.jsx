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
  {id: 'average_memory', label: 'Average memory', unit: 'GB', value: point => point.metrics?.memory?.average_bytes / 1_000_000_000},
  {id: 'maximum_memory', label: 'Maximum memory', unit: 'GB', value: point => point.metrics?.memory?.maximum_bytes / 1_000_000_000},
  {id: 'lock_wait', label: 'Lock wait', unit: 'µs', value: point => point.metrics?.locks?.wait_us},
  {id: 'lock_collision', label: 'Lock collisions', unit: '%', value: point => point.metrics?.locks?.collision_percent},
  {id: 'lock_duration', label: 'Lock duration', unit: '%', value: point => point.metrics?.locks?.duration_percent},
  {id: 'net_send_count', label: 'Socket writes', unit: '', value: point => point.metrics?.network?.send_count},
  {id: 'net_send_octets', label: 'Bytes sent', unit: 'GB', value: point => point.metrics?.network?.send_octets / 1_000_000_000},
  {id: 'net_packet', label: 'Avg packet', unit: 'B', value: point => point.metrics?.network?.average_packet_bytes},
  {id: 'net_send_pending', label: 'Max send pending', unit: 'MB', value: point => point.metrics?.network?.send_pending?.maximum_bytes / 1_000_000},
  {id: 'net_queue', label: 'Max port queue', unit: 'MB', value: point => point.metrics?.network?.port_queue_size?.maximum_bytes / 1_000_000},
  {id: 'net_port_memory', label: 'Max port memory', unit: 'MB', value: point => point.metrics?.network?.port_memory?.maximum_bytes / 1_000_000},
  {id: 'net_busy_events', label: 'Busy-port suspensions', unit: '', value: point => point.metrics?.network?.busy_dist_port_events},
  {id: 'net_busy_writers', label: 'Suspended writers', unit: '', value: point => point.metrics?.network?.busy_dist_port_writers}
];

function configKey(point) {
  return [
    point.operation,
    point.payload,
    point.messages_per_writer,
    point.pace_ms,
    point.distribution_busy_limit_kib,
    point.sender_config,
    point.receiver_config
  ].join('|');
}

function runAnchor(run) {
  return `run-${run.id}`;
}

function runConfig(run) {
  const point = run.points[0];
  return {
    sender: point?.sender_config ?? 'Unavailable',
    receiver: point?.receiver_config ?? 'Unavailable',
    busyLimit: point === undefined
      ? 'Unavailable'
      : `${point.distribution_busy_limit_kib} KiB`
  };
}

function runDateTime(run) {
  const match = run.id.match(/\d{4}-\d{2}-\d{2}_\d{2}\.\d{2}\.\d{2}$/);
  return match?.[0] ?? run.id;
}

function RunRow({run}) {
  const config = runConfig(run);
  return (
    <tr>
      <th scope="row">
        <a href={`#${runAnchor(run)}`}>{runDateTime(run)}</a>
      </th>
      <td>{config.sender}</td>
      <td>{config.receiver}</td>
      <td>{config.busyLimit}</td>
    </tr>
  );
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
        senderConfig: point.sender_config ?? 'Unavailable',
        receiverConfig: point.receiver_config ?? 'Unavailable',
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
  const maximumFractionDigits = unit === 'GB' ? 3 : 2;
  const formatted = new Intl.NumberFormat(
    undefined,
    {maximumFractionDigits}
  ).format(value);
  return unit === '' ? formatted : `${formatted} ${unit}`;
}

function MetricGrid({group, writerCounts}) {
  return (
    <div className="table-scroll">
      <table>
        <thead>
          <tr>
            <th>Metric</th>
            <th>Path</th>
            {writerCounts.map(count => <th key={count}>{count.toLocaleString()}</th>)}
          </tr>
        </thead>
        <tbody>
          {metrics.flatMap(metric => paths.map((path, pathIndex) => (
            <tr
              className={pathIndex === paths.length - 1 ? 'metric-row-end' : undefined}
              key={`${metric.id}.${path}`}
            >
              {pathIndex === 0 && (
                <th className="metric-name" rowSpan={paths.length}>
                  {metric.label}
                </th>
              )}
              <th className="path-name">
                <span className={`path-marker ${path}`} />{path}
              </th>
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
    plugins: {
      legend: {position: 'bottom'},
      tooltip: {
        callbacks: {
          label: context =>
            `${context.dataset.label}: ${formatValue(context.parsed.y, metric.unit)}`
        }
      }
    },
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

function PointGroup({group, dateTime}) {
  const writerCounts = [...new Set(group.points.map(point => point.writer_count))]
    .sort((left, right) => left - right);
  return (
    <section className="point-group">
      <header>
        <div>
          <span className="eyebrow">{group.operation}</span>
          <h3>{group.payload} payload</h3>
        </div>
        <dl className="config">
          <div><dt>Date/time</dt><dd>{dateTime}</dd></div>
          <div><dt>Messages / writer</dt><dd>{group.messagesPerWriter}</dd></div>
          <div><dt>Pace</dt><dd>{group.paceMs} ms</dd></div>
          <div><dt>Busy limit</dt><dd>{group.busyLimitKiB} KiB</dd></div>
          <div><dt>Sender</dt><dd>{group.senderConfig}</dd></div>
          <div><dt>Receiver</dt><dd>{group.receiverConfig}</dd></div>
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

function RunErrors({errors}) {
  if (errors.length === 0) return null;
  return (
    <details className="alert" open>
      <summary>{errors.length} point file(s) could not be parsed</summary>
      <ul>
        {errors.map(error => (
          <li key={error.file}><code>{error.file}</code>: {error.message}</li>
        ))}
      </ul>
    </details>
  );
}

function RunSection({run, anchor}) {
  const groups = useMemo(() => groupPoints(run.points), [run.points]);
  const dateTime = runDateTime(run);
  return (
    <section className="run-section" id={anchor}>
      <header className="run-header">
        <h2>Performance run</h2>
        <a href={run.report_url}>Open Common Test report ↗</a>
      </header>
      <RunErrors errors={run.errors} />
      {groups.length === 0
        ? <div className="empty"><h3>No completed points in this run</h3></div>
        : groups.map(group => (
          <PointGroup
            key={configKey(group.points[0])}
            group={group}
            dateTime={dateTime}
          />
        ))}
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
  const [loadError, setLoadError] = useState(null);
  const [updatedAt, setUpdatedAt] = useState(null);

  async function refresh() {
    try {
      const response = await fetch('/api/report');
      if (!response.ok) throw new Error(`HTTP ${response.status}`);
      const nextReport = await response.json();
      setReport(nextReport);
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

  return (
    <main>
      <header className="page-header">
        <div>
          <span className="eyebrow">Common Test metrics</span>
          <h1>ecall performance</h1>
          <p>Native Erlang distribution and ecall, compared at each writer count.</p>
        </div>
        <div className="toolbar">
          <button type="button" onClick={refresh}>Refresh</button>
        </div>
      </header>

      <div className="status">
        <span>{updatedAt ? `Updated ${updatedAt.toLocaleTimeString()}` : 'Loading…'}</span>
        <span>{report.runs.length} run(s)</span>
      </div>

      {loadError && <div className="alert">Could not load report data: {loadError}</div>}

      {report.runs.length === 0
        ? <EmptyState />
        : <>
          <nav className="contents" aria-label="Performance runs">
            <h2>Runs</h2>
            <div className="table-scroll">
              <table className="runs-grid">
                <thead>
                  <tr>
                    <th>Date/time</th>
                    <th>Sender</th>
                    <th>Receiver</th>
                    <th>Busy limit</th>
                  </tr>
                </thead>
                <tbody>
                  {report.runs.map(run => (
                    <RunRow key={run.id} run={run} />
                  ))}
                </tbody>
              </table>
            </div>
          </nav>
          {report.runs.map(run => (
            <RunSection
              key={run.id}
              run={run}
              anchor={runAnchor(run)}
            />
          ))}
        </>}
    </main>
  );
}
