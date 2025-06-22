#!/usr/bin/env python3
import argparse
import os
from collections import defaultdict
from Bio import SeqIO
from html import escape

def read_fasta(input_fasta):
    """Read sequences from fasta, returns list of SeqRecord"""
    return list(SeqIO.parse(input_fasta, "fasta"))

def group_variants(records):
    """
    Group duplicate sequences and merge headers as variant names
    Returns a list of tuples: (merged_header, sequence)
    """
    seq_dict = defaultdict(list)  # seq string -> list of headers

    for rec in records:
        seq = str(rec.seq)
        seq_dict[seq].append(rec.id)

    grouped = []
    for seq, headers in seq_dict.items():
        if len(headers) > 1:
            merged_header = "variant|" + "|".join(headers)
        else:
            merged_header = headers[0]
        grouped.append((merged_header, seq))

    return grouped

def write_fasta(grouped, output_fasta):
    with open(output_fasta, "w") as f:
        for header, seq in grouped:
            f.write(f">{header}\n")
            # wrap sequences at 80 chars per line (optional)
            for i in range(0, len(seq), 80):
                f.write(seq[i:i+80] + "\n")

def highlight_alignment(seq1, seq2):
    """Return HTML strings highlighting matches and mismatches"""
    q_html, s_html = "", ""
    for q, s in zip(seq1, seq2):
        if q == s:
            style = 'color:black'
        elif q == '-' or s == '-':
            style = 'color:red'
        else:
            style = 'color:orange'
        q_html += f'<span style="{style}">{escape(q)}</span>'
        s_html += f'<span style="{style}">{escape(s)}</span>'
    return q_html, s_html

def generate_html(grouped, output_html):
    """Generate a simple HTML showing all sequences aligned to the first sequence"""
    if not grouped:
        print("No sequences to write HTML for.")
        return

    # Use first sequence as reference for alignment
    ref_header, ref_seq = grouped[0]

    with open(output_html, "w") as f:
        f.write("<html><head><style>body { font-family: monospace; }</style></head><body>\n")
        f.write(f"<h1>Multiple Sequence Alignment (reference: {ref_header})</h1>\n")

        for header, seq in grouped:
            q_html, s_html = highlight_alignment(ref_seq, seq)
            f.write(f"<h3>{header}</h3>\n<pre>\n")
            f.write(f"Ref: {q_html}<br>\n")
            f.write(f"Seq: {s_html}<br>\n</pre><hr>\n")

        f.write("</body></html>\n")

def main():
    parser = argparse.ArgumentParser(description="Group duplicate sequences and optionally generate alignment HTML.")
    parser.add_argument("-i", "--input", required=True, help="Input FASTA file")
    parser.add_argument("-o", "--output-dir", default="blastn_summary", help="Output directory (default: blastn_summary)")
    parser.add_argument("-p", "--prefix", default="variants", help="Prefix for output files (default: variants)")
    parser.add_argument("--html", action="store_true", help="Generate HTML alignment report")
    args = parser.parse_args()

    os.makedirs(args.output_dir, exist_ok=True)

    # Read sequences
    records = read_fasta(args.input)
    if not records:
        print("No sequences found in the input fasta file.")
        return

    # Group variants
    grouped = group_variants(records)

    # Write unique sequences fasta
    out_fasta = os.path.join(args.output_dir, f"{args.prefix}_unique.fasta")
    write_fasta(grouped, out_fasta)
    print(f"✅ Unique sequences saved to: {out_fasta}")

    # Generate HTML if requested
    if args.html:
        out_html = os.path.join(args.output_dir, f"{args.prefix}_alignment.html")
        generate_html(grouped, out_html)
        print(f"✅ HTML alignment report saved to: {out_html}")

if __name__ == "__main__":
    main()

#Usage
#python align_and_group_variants.py -i input_sequences.fasta -o results_folder -p my_variants --html
