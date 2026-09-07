#!/usr/bin/env python3
"""Generate the article figures as standalone SVG from the September 2026
performance JSON (schema 7). No third-party dependencies.

    python3 make_figures.py [results_root]

Writes send-native.svg, send-native-vs-ecall.svg and cast-call.svg next to
this script.
"""
import glob
import json
import os
import sys

ROOT = sys.argv[1] if len(sys.argv) > 1 else "/home/roman/WORKTEMP/202609/ecall/tests"
OUT = os.path.dirname(os.path.abspath(__file__))

# Palette (validated with the dataviz reference validator, light surface).
SURFACE = "#fcfcfb"
INK = "#0b0b0b"
INK2 = "#52514e"
MUTED = "#898781"
GRID = "#e1e0d9"
AXIS = "#c3c2b7"
NATIVE = "#2a78d6"   # categorical slot 1
ECALL = "#eb6834"    # categorical slot 2
FONT = 'system-ui, -apple-system, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif'

Y_MAX = 1_600_000
Y_TICKS = [0, 250_000, 500_000, 750_000, 1_000_000, 1_250_000, 1_500_000]
SOURCE_LINE = ("September 2026 runs: two 48-core hosts, OTP 27, 1,000 messages per writer, "
               "100 ms pace, 225-byte map payload, +zdbbl 1024")


def load():
    rows = {}
    pattern = os.path.join(ROOT, "**", "performance_data", "*.json")
    for f in glob.glob(pattern, recursive=True):
        with open(f) as fh:
            d = json.load(fh)
        rows[(d["operation"], d["path"], d["writer_count"])] = d
    if not rows:
        sys.exit(f"no results under {ROOT}")
    return rows


def throughput(rows, op, path):
    pts = []
    for (o, p, w), d in rows.items():
        if o == op and p == path:
            pts.append((w, w * d["messages_per_writer"] / (d["elapsed_ms"] / 1000.0)))
    return sorted(pts)


def intended(writers):
    return [(w, w * 10.0) for w in writers]


def fmt_k(v):
    if v >= 1_000_000:
        s = f"{v / 1_000_000:.2f}".rstrip("0").rstrip(".")
        return f"{s}M"
    if v >= 1000:
        return f"{round(v / 1000):d}k"
    return f"{v:.0f}"


def fmt_tick(v):
    if v == 0:
        return "0"
    if v >= 1_000_000:
        s = f"{v / 1_000_000:.2f}".rstrip("0").rstrip(".")
        return f"{s}M"
    return f"{v // 1000}k"


def esc(s):
    return s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")


class Panel:
    def __init__(self, x, y, w, h, x_min, x_max, x_ticks, title=None, y_labels=True):
        self.x, self.y, self.w, self.h = x, y, w, h
        self.x_min, self.x_max = x_min, x_max
        self.x_ticks = x_ticks
        self.title = title
        self.y_labels = y_labels

    def px(self, v):
        return self.x + (v - self.x_min) / (self.x_max - self.x_min) * self.w

    def py(self, v):
        return self.y + self.h - v / Y_MAX * self.h

    def frame(self):
        out = []
        if self.title:
            out.append(f'<text x="{self.x}" y="{self.y - 10}" font-size="13" font-weight="600" '
                       f'fill="{INK}">{esc(self.title)}</text>')
        for t in Y_TICKS:
            yy = self.py(t)
            out.append(f'<line x1="{self.x}" y1="{yy:.1f}" x2="{self.x + self.w}" y2="{yy:.1f}" '
                       f'stroke="{GRID}" stroke-width="1"/>')
            if self.y_labels:
                out.append(f'<text x="{self.x - 8}" y="{yy + 4:.1f}" font-size="11" fill="{MUTED}" '
                           f'text-anchor="end" font-variant-numeric="tabular-nums">{fmt_tick(t)}</text>')
        # baseline
        out.append(f'<line x1="{self.x}" y1="{self.py(0):.1f}" x2="{self.x + self.w}" y2="{self.py(0):.1f}" '
                   f'stroke="{AXIS}" stroke-width="1"/>')
        for t in self.x_ticks:
            xx = self.px(t)
            out.append(f'<line x1="{xx:.1f}" y1="{self.py(0):.1f}" x2="{xx:.1f}" y2="{self.py(0) + 4:.1f}" '
                       f'stroke="{AXIS}" stroke-width="1"/>')
            out.append(f'<text x="{xx:.1f}" y="{self.py(0) + 17:.1f}" font-size="11" fill="{MUTED}" '
                       f'text-anchor="middle" font-variant-numeric="tabular-nums">{fmt_tick(t)}</text>')
        return out

    def reference(self, pts, label):
        d = " ".join(f"{'M' if i == 0 else 'L'}{self.px(w):.1f},{self.py(v):.1f}" for i, (w, v) in enumerate(pts))
        out = [f'<path d="{d}" fill="none" stroke="{MUTED}" stroke-width="1.5" stroke-linecap="round" '
               f'stroke-linejoin="round"/>']
        w, v = pts[-1]
        out.append(f'<text x="{self.px(w) + 10:.1f}" y="{self.py(v) + 4:.1f}" font-size="11" '
                   f'fill="{MUTED}">{esc(label)}</text>')
        return out

    def series(self, pts, color, end_label, label_dy=0):
        d = " ".join(f"{'M' if i == 0 else 'L'}{self.px(w):.1f},{self.py(v):.1f}" for i, (w, v) in enumerate(pts))
        out = [f'<path d="{d}" fill="none" stroke="{color}" stroke-width="2" stroke-linecap="round" '
               f'stroke-linejoin="round"/>']
        for w, v in pts:
            cx, cy = self.px(w), self.py(v)
            out.append(f'<circle cx="{cx:.1f}" cy="{cy:.1f}" r="6" fill="{SURFACE}"/>')
            out.append(f'<circle cx="{cx:.1f}" cy="{cy:.1f}" r="4" fill="{color}"/>')
        w, v = pts[-1]
        x0, y0 = self.px(w) + 10, self.py(v) + 4 + label_dy
        out.append(f'<circle cx="{x0 + 3:.1f}" cy="{y0 - 4:.1f}" r="3" fill="{color}"/>')
        out.append(f'<text x="{x0 + 10:.1f}" y="{y0:.1f}" font-size="12" fill="{INK2}" '
                   f'font-weight="600">{esc(end_label)}</text>')
        return out


def legend(x, y, items):
    out = []
    cx = x
    for color, name in items:
        if color is None:
            out.append(f'<line x1="{cx}" y1="{y - 4}" x2="{cx + 18}" y2="{y - 4}" stroke="{MUTED}" '
                       f'stroke-width="1.5"/>')
        else:
            out.append(f'<line x1="{cx}" y1="{y - 4}" x2="{cx + 18}" y2="{y - 4}" stroke="{color}" '
                       f'stroke-width="2" stroke-linecap="round"/>')
            out.append(f'<circle cx="{cx + 9}" cy="{y - 4}" r="3.5" fill="{color}"/>')
        out.append(f'<text x="{cx + 24}" y="{y}" font-size="12" fill="{INK2}">{esc(name)}</text>')
        cx += 24 + 7 * len(name) + 22
    return out


def svg_doc(width, height, title, subtitle, body):
    parts = [
        f'<svg xmlns="http://www.w3.org/2000/svg" width="{width}" height="{height}" '
        f'viewBox="0 0 {width} {height}" font-family=\'{FONT}\' role="img" aria-label="{esc(title)}">',
        f'<rect width="{width}" height="{height}" fill="{SURFACE}"/>',
        f'<text x="24" y="30" font-size="16" font-weight="600" fill="{INK}">{esc(title)}</text>',
        f'<text x="24" y="48" font-size="12" fill="{INK2}">{esc(subtitle)}</text>',
    ]
    parts.extend(body)
    parts.append(f'<text x="24" y="{height - 12}" font-size="10" fill="{MUTED}">{esc(SOURCE_LINE)}</text>')
    parts.append("</svg>")
    return "\n".join(parts) + "\n"


def write(name, doc):
    path = os.path.join(OUT, name)
    with open(path, "w") as fh:
        fh.write(doc)
    print("wrote", path)


def fig_send_native(rows):
    native = throughput(rows, "send", "native")
    writers = [w for w, _ in native]
    W, H = 760, 460
    p = Panel(70, 100, 540, 280, 0, 160_000, list(range(20_000, 160_001, 20_000)))
    body = legend(24, 76, [(NATIVE, "native RemotePid ! Msg"), (None, "intended pace (10 msg/s per writer)")])
    body += p.frame()
    body += p.reference(intended(writers), "intended")
    body += p.series(native, NATIVE, f"{fmt_k(native[-1][1])} msg/s measured")
    body.append(f'<text x="{p.x + p.w / 2:.0f}" y="{p.y + p.h + 36}" font-size="11" fill="{INK2}" '
                f'text-anchor="middle">writer processes on the sending node</text>')
    body.append(f'<text transform="translate(18 {p.y + p.h / 2:.0f}) rotate(-90)" font-size="11" '
                f'fill="{INK2}" text-anchor="middle">messages per second</text>')
    write("send-native.svg",
          svg_doc(W, H, "Distributed send throughput stops scaling past 30,000 senders",
                  "Plain distributed send, one receiver process per writer, one node pair", body))


def fig_send_compare(rows):
    native = throughput(rows, "send", "native")
    ecall = throughput(rows, "send", "ecall")
    writers = [w for w, _ in native]
    W, H = 760, 460
    p = Panel(70, 100, 540, 280, 0, 160_000, list(range(20_000, 160_001, 20_000)))
    body = legend(24, 76, [(NATIVE, "native RemotePid ! Msg"), (ECALL, "ecall:send/2"), (None, "intended pace")])
    body += p.frame()
    body += p.reference(intended(writers), "")
    body += p.series(native, NATIVE, f"{fmt_k(native[-1][1])} msg/s")
    body += p.series(ecall, ECALL, f"{fmt_k(ecall[-1][1])} msg/s")
    body.append(f'<text x="{p.x + p.w / 2:.0f}" y="{p.y + p.h + 36}" font-size="11" fill="{INK2}" '
                f'text-anchor="middle">writer processes on the sending node</text>')
    body.append(f'<text transform="translate(18 {p.y + p.h / 2:.0f}) rotate(-90)" font-size="11" '
                f'fill="{INK2}" text-anchor="middle">messages per second</text>')
    write("send-native-vs-ecall.svg",
          svg_doc(W, H, "Send: native distribution against the ecall pool, same node pair, same socket",
                  "Messages per second against writer count; the gray line is the intended pace", body))


def fig_cast_call(rows):
    W, H = 760, 460
    ticks = [10_000, 50_000, 100_000, 150_000]
    left = Panel(70, 122, 230, 258, 0, 160_000, ticks, title="cast: erpc:cast/4 vs ecall:cast/4")
    right = Panel(410, 122, 230, 258, 0, 160_000, ticks, title="call: erpc:call/4 vs ecall:call/4",
                  y_labels=False)
    body = legend(24, 76, [(NATIVE, "native erpc"), (ECALL, "ecall")])
    for panel, op in ((left, "cast"), (right, "call")):
        native = throughput(rows, op, "native")
        ecall = throughput(rows, op, "ecall")
        body += panel.frame()
        body += panel.series(native, NATIVE, fmt_k(native[-1][1]))
        body += panel.series(ecall, ECALL, fmt_k(ecall[-1][1]))
        body.append(f'<text x="{panel.x + panel.w / 2:.0f}" y="{panel.y + panel.h + 36}" font-size="11" '
                    f'fill="{INK2}" text-anchor="middle">writer processes</text>')
    body.append(f'<text transform="translate(18 {left.y + left.h / 2:.0f}) rotate(-90)" font-size="11" '
                f'fill="{INK2}" text-anchor="middle">operations per second</text>')
    write("cast-call.svg",
          svg_doc(W, H, "Cast and call: the same transport, the same knee, one scale",
                  "Completed operations per second against writer count", body))


if __name__ == "__main__":
    rows = load()
    fig_send_native(rows)
    fig_send_compare(rows)
    fig_cast_call(rows)
