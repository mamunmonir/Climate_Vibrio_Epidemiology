import pandas as pd
import glob
import os
import argparse
from html import escape

# Set up argument parser
parser = argparse.ArgumentParser(description="Summarize BLASTN results and visualize best hits.")
parser.add_argument('--input-dir', required=True, help='Directory containing BLASTN .tsv files')
parser.add_argument('--all-tsv', required=True, help='Output filename for all hits (.tsv)')
parser.add_argument('--best-tsv', required=True, help='Output filename for best hits (.tsv)')
parser.add_argument('--html', required=True, help='Output filename for HTML alignment report (.html)')
args = parser.parse_args()

# BLAST outfmt 6 + qseq + sseq columns
columns = [
    "qseqid", "sseqid", "pident", "length", "qlen", "mismatch", "gapopen",
    "qstart", "qend", "sstart", "send", "evalue", "bitscore", "qseq", "sseq"
]

# Find all TSV files
files = glob.glob(os.path.join(args.input_dir, "*.tsv"))
if not files:
    raise FileNotFoundError(f"No .tsv files found in {args.input_dir}")

# Read and combine
dfs = []
for file in files:
    df = pd.read_csv(file, sep='\t', header=None)
    df.columns = columns
    df['source_file'] = os.path.basename(file)
    dfs.append(df)

all_hits = pd.concat(dfs, ignore_index=True)

# Save all hits
all_hits_path = os.path.join(args.input_dir, args.all_tsv)
all_hits.to_csv(all_hits_path, sep='\t', index=False)
print(f"✅ All hits saved to: {all_hits_path}")

# Best hit per query
best_hits = all_hits.sort_values(["qseqid", "bitscore"], ascending=[True, False])
best_hits = best_hits.drop_duplicates(subset="qseqid", keep="first")

# Save best hits
best_hits_path = os.path.join(args.input_dir, args.best_tsv)
best_hits.to_csv(best_hits_path, sep='\t', index=False)
print(f"✅ Best hits saved to: {best_hits_path}")

# Highlight function
def highlight_alignment(qseq, sseq):
    q_html, s_html = "", ""
    for q, s in zip(qseq, sseq):
        if q == s:
            style = 'color:black'
        elif q == '-' or s == '-':
            style = 'color:red'
        else:
            style = 'color:orange'
        q_html += f'<span style="{style}">{escape(q)}</span>'
        s_html += f'<span style="{style}">{escape(s)}</span>'
    return q_html, s_html

# Write HTML
html_path = os.path.join(args.input_dir, args.html)
with open(html_path, "w") as f:
    f.write("<html><head><style>body { font-family: monospace; }</style></head><body>")
    f.write("<h1>Best Hit Alignments</h1>")

    for _, row in best_hits.iterrows():
        qid = row['qseqid']
        sid = row['sseqid']
        pident = row['pident']
        qseq = row['qseq']
        sseq = row['sseq']

        q_html, s_html = highlight_alignment(qseq, sseq)

        f.write(f"<h3>Query: {qid} | Subject: {sid} | Identity: {pident:.2f}%</h3>")
        f.write("<pre>")
        f.write(f"Q: {q_html}<br>")
        f.write(f"S: {s_html}<br>")
        f.write("</pre><hr>")

    f.write("</body></html>")

print(f"✅ HTML alignment report saved to: {html_path}")
