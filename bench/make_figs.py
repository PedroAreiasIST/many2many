#!/usr/bin/env python3
"""make_figs.py -- the comparison figures of the paper (Section 4) from the stored results_*.jsonl in this folder.
Needs gnuplot and figtex2pdf.sh (fig terminal with LaTeX labels -> <stem>.pdf beside the script); with another terminal
(e.g. `set terminal pdfcairo`) the .gp files it writes render without LaTeX. Never edit the .gp by hand.
    cross_times      Tables 24-26: transpose and product, time against elements, four codes, three machines
    gap_times        Table 28: breadth-first search and components, library against the GAP suite, three machines
    derived_speedup  Table 27: SciPy time over library time for four derived queries on the laptop"""
import json, os, subprocess, collections
HERE = os.path.dirname(os.path.abspath(__file__)); B = HERE  # the results_*.jsonl beside this script
COLORS = ["#000000", "#E69F00", "#0072B2", "#009E73", "#D55E00", "#CC79A7", "#56B4E9"]
PTS_FILLED = [7, 5, 9, 13, 15, 11, 3]; PTS_OPEN = [6, 4, 8, 12, 14, 10, 2]
def head(stem, size, fontsize):
    s = 'set terminal fig color big textspecial fontsize %d linewidth 2.0 depth 10 size %s\nset output "%s.fig"\n' % (fontsize, size, stem)
    for i, (c, pf, po) in enumerate(zip(COLORS, PTS_FILLED, PTS_OPEN)):
        s += 'set linetype %d lw 2 pt %d ps 1.0 lc rgb "%s"\n' % (i + 1, pf, c)       # 1..7 filled markers
        s += 'set linetype %d lw 2 pt %d ps 1.0 lc rgb "%s"\n' % (i + 11, po, c)      # 11..17 open markers
    return s
def rows(name):
    return [json.loads(l) for l in open(os.path.join(B, name)) if l.startswith('{')]
def render(stem, body):
    open(os.path.join(HERE, stem + '.gp'), 'w').write(body + 'unset output\n')
    subprocess.run(['gnuplot', stem + '.gp'], cwd=HERE, check=True)
    subprocess.run(['figtex2pdf.sh', stem + '.fig'], cwd=HERE, check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    for ext in ('.aux', '.log', '.tex', '.dvi', '.ps'):
        p = os.path.join(HERE, stem + ext)
        if os.path.exists(p): os.remove(p)
    print('  figs/%s.pdf' % stem)
def block(name, pts):
    return '$%s << EOD\n' % name + ''.join('%g %g\n' % (x, y) for x, y in pts) + 'EOD\n'

MACHINES = [('laptop', 'Laptop (8 cores)', 16), ('amd2', 'Desktop (16 cores)', 16), ('deucalion', 'Node (128 cores)', 128)]
XT = 'set xtics ("1" 1e6, "8" 8e6, "27" 27e6, "64" 64e6)\nset xrange [0.75e6:9e7]\nset logscale xy\nset format y "$10^{%T}$"\nset mxtics 1\n'

def cross_times():
    stem = 'cross_times'; body = head(stem, '10.5,6.6', 16)
    body += 'set multiplot\n' + XT + 'set xlabel "elements [$\\\\times10^{6}$]"\nset grid ytics\n'
    data = ''; plots = ''; keyentries = None
    W, H, Y0 = 0.3333, 0.43, 0.0
    for col, (label, title, thr) in enumerate(MACHINES):
        R = rows('results_chain_%s.jsonl' % label)
        by = collections.defaultdict(dict)
        for r in R: by[(r['threads'], r['nee'])][r['backend']] = (r['nel'], r['t_transpose'], r['t_product'])
        for row, op in enumerate(('t', 'p')):
            k = 1 + row
            def series(th, pick):
                out = []
                for nee in sorted({n for (t, n) in by if t == th}):
                    d = by[(th, nee)]; v = pick(d)
                    if v is not None: out.append((d[next(iter(d))][0], v))
                return out
            sel = lambda names: (lambda d: min(d[n][k] for n in names if n in d) if any(n in d for n in names) else None)
            lib = ['library-icpx', 'library-gcc']; fort = ['fortran-ifx', 'fortran-gfortran']
            specs = [('lib_p', series(thr, sel(lib)), 1, 'This library, %d thr.' % thr, 'dt 1'),
                     ('gb_p', series(thr, sel(['graphblas'])), 2, 'GraphBLAS, %d thr.' % thr, 'dt 1'),
                     ('f_p', series(thr, sel(fort)), 3, 'Fortran, %d thr.' % thr, 'dt 1'),
                     ('lib_1', series(1, sel(lib)), 11, 'This library, 1 thr.', 'dt 2'),
                     ('gb_1', series(1, sel(['graphblas'])), 12, 'GraphBLAS, 1 thr.', 'dt 2'),
                     ('f_1', series(1, sel(fort)), 13, 'Fortran, 1 thr.', 'dt 2'),
                     ('sp_1', series(1, sel(['scipy'])), 14, 'SciPy, 1 thr.', 'dt 2')]
            names = []
            for nm, pts, lt, key, dt in specs:
                full = '%s_%s_%s' % (label, op, nm); data += block(full, pts)
                names.append('$%s u 1:2 w lp lt %d %s notitle' % (full, lt, dt))
            ylabel = 'Transpose $A^{T}$ [s]' if op == 't' else 'Product $A^{T}\\\\cdot A$ [s]'
            yr = '[3e-3:40]' if op == 't' else '[8e-3:80]'
            ttl = 'set title "%s"\n' % (title if row == 0 else '')
            plots += 'set size %g,%g\nset origin %g,%g\n' % (W, H, col * W, Y0 + (1 - row) * H) + ttl + 'set ylabel "%s"\nset yrange %s\nplot %s\n' % (ylabel, yr, ', '.join(names))
    # the key alone, across the full width above the panels (the thread count of the parallel rows is per machine)
    keys = [('1 dt 1', 'This library, parallel'), ('2 dt 1', 'GraphBLAS, parallel'), ('3 dt 1', 'Fortran, parallel'),
            ('11 dt 2', 'This library, 1 thread'), ('12 dt 2', 'GraphBLAS, 1 thread'), ('13 dt 2', 'Fortran, 1 thread'), ('14 dt 2', 'SciPy, 1 thread')]
    kplot = 'unset logscale\nunset xtics\nunset ytics\nunset xlabel\nunset ylabel\nunset title\nunset grid\nunset border\nset xrange [0:1]\nset yrange [0:1]\n'
    kplot += 'set size 1,0.14\nset origin 0,0.86\nset key center center horizontal maxcols 4 Left reverse nobox samplen 2.5 spacing 1.15 width -2\n'
    kplot += 'plot ' + ', '.join('NaN w lp lt %s t "%s"' % (lt, t) for lt, t in keys) + '\n'
    render(stem, body + data + plots + kplot + 'unset multiplot\n')

def gap_times():
    stem = 'gap_times'; body = head(stem, '9.5,4.2', 15)
    body += 'set multiplot layout 1,2\n' + XT
    body += 'set xlabel "elements [$\\\\times10^{6}$]"\nset grid ytics\n'
    data = ''; plots = ''
    TH = {'laptop': 16, 'amd2': 32, 'deucalion': 128}
    for pi, (op, gkey, lkey, ylabel) in enumerate((('bfs', 't_bfs', 't_bfs', 'Breadth-first search [s]'), ('cc', 't_cc', 't_components_nodes', 'Connected components [s]'))):
        names = []
        for mi, (label, title, _) in enumerate(MACHINES):
            th = TH[label]
            G = {(r['nee']): (r['nel'], r[gkey]) for r in rows('results_gap_%s.jsonl' % label) if r['threads'] == th}
            L = {(r['nee']): (r['nel'], r[lkey]) for r in rows('results_graph_%s.jsonl' % label) if r.get('backend') == 'library' and r['threads'] == th and lkey in r}
            lp = [(L[n][0], L[n][1]) for n in sorted(L) if n in G]; gp = [(G[n][0], G[n][1]) for n in sorted(G) if n in L]
            fl = '%s_%s_lib' % (label, op); fg = '%s_%s_gap' % (label, op); data += block(fl, lp) + block(fg, gp)
            short = title.split(' (')[0]
            names.append('$%s u 1:2 w lp lt %d dt 1 t "This library, %s, %d thr."' % (fl, mi + 1, short.lower(), th))
            names.append('$%s u 1:2 w lp lt %d dt 2 t "GAP, %s, %d thr."' % (fg, mi + 11, short.lower(), th))
        keyset = 'set key inside left top Left reverse nobox spacing 1.1 samplen 2\n' if pi == 1 else 'unset key\n'
        plots += keyset + 'set ylabel "%s"\nset yrange [2e-3:20]\nplot %s\n' % (ylabel, ', '.join(names))
    render(stem, body + data + plots + 'unset multiplot\n')

def derived_speedup():
    stem = 'derived_speedup'; body = head(stem, '7,4.2', 15)
    body += XT.replace('set xrange [0.75e6:9e7]', 'set xrange [0.75e6:4e7]')
    body += 'set xlabel "elements [$\\\\times10^{6}$]"\nset ylabel "SciPy time / library time"\nset grid ytics\nset yrange [0.5:3000]\n'
    body += 'set key inside left top Left reverse nobox spacing 1.1 samplen 2 horizontal maxcols 2 width -4 font ",13"\n'
    R = rows('results_graph_laptop.jsonl'); S = {r['nee']: r for r in R if r.get('backend') == 'scipy'}
    Q = [('t_rcm', 'Cuthill--McKee'), ('t_components_nodes', 'components'), ('t_bfs', 'breadth-first levels'), ('t_dual_faces', 'dual graph')]
    data = ''; names = []
    for qi, (q, nm) in enumerate(Q):
        for th, lt, dt in ((16, qi + 1, 'dt 1'), (1, qi + 11, 'dt 2')):
            pts = [(r['nel'], S[r['nee']][q] / r[q]) for r in sorted(R, key=lambda r: r['nee']) if r.get('backend') == 'library' and r['threads'] == th and q in r and r['nee'] in S]
            full = 'd_%s_%d' % (q, th); data += block(full, pts)
            names.append('$%s u 1:2 w lp lt %d %s t "%s, %d thr."' % (full, lt, dt, nm, th))
    render(stem, body + data + 'plot 1 w l lt 1 dt 3 lw 1 notitle, ' + ', '.join(names) + '\n')

if __name__ == '__main__':
    import sys
    which = sys.argv[1:] or ['cross_times', 'gap_times', 'derived_speedup']
    for w in which: globals()[w]()
