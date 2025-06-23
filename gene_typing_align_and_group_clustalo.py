#!/usr/bin/env python3
import argparse
import os
import subprocess
from collections import defaultdict
from Bio import SeqIO
from html import escape
from itertools import zip_longest

def run_clustalo(input_fasta, output_fasta):
    """Run Clustal Omega to align sequences"""
    try:
        subprocess.run([
            "clustalo", "-i", input_fasta, "-o", output_fasta,
            "--force", "--outfmt=fasta"
        ], stderr=subprocess.DEVNULL, check=True)
        print(f"✅ Alignment completed with Clustal Omega: {output_fasta}")
    except subprocess.CalledProcessError:
        print("❌ Clustal Omega failed. Make sure 'clustalo' is installed and in your PATH.")
        exit(1)

def read_fasta(path):
    return list(SeqIO.parse(path, "fasta"))

def group_variants(records):
    """Group identical aligned sequences and merge headers"""
    seq_dict = defaultdict(list)
    for rec in records:
        seq_dict[str(rec.seq)].append(rec.id)

    grouped = []
    for seq, headers in seq_dict.items():
        merged_header = "variant|" + "|".join(headers) if len(headers) > 1 else headers[0]
        grouped.append((merged_header, seq))
    return grouped

def write_fasta(grouped, output_path):
    """Write grouped sequences to FASTA"""
    with open(output_path, "w") as f:
        for header, seq in grouped:
            f.write(f">{header}\n")
            for i in range(0, len(seq), 80):
                f.write(seq[i:i+80] + "\n")

def highlight_alignment(ref_seq, test_seq):
    """Return HTML-highlighted sequence alignment"""
    ref_html, test_html = "", ""
    for r, t in zip_longest(ref_seq, test_seq, fillvalue='-'):
        if r == t:
            style = "color:black"
        elif r == '-' or t == '-':
            style = "color:red"
        else:
            style = "color:orange"
        ref_html += f"<span style='{style}'>{escape(r)}</span>"
        test_html += f"<span style='{style}'>{escape(t)}</span>"
    return ref_html, test_html

def generate_html(grouped, output_path):
    """Generate alignment HTML report from aligned sequences"""
    if not grouped:
        print("No sequences to write HTML for.")
        return

    ref_header, ref_seq = grouped[0]
    with open(output_path, "w") as f:
        f.write("<html><head><style>body { font-family: monospace; }</style></head><body>\n")
        f.write(f"<h1>Multiple Sequence Alignment (reference: {ref_header})</h1>\n")
        f.write("<p><b>Legend:</b> <span style='color:black'>Match</span>, "
                "<span style='color:orange'>Mismatch</span>, "
                "<span style='color:red'>Gap</span></p>\n")

        for header, seq in grouped:
            ref_html, test_html = highlight_alignment(ref_seq, seq)
            f.write(f"<h3>{header}</h3>\n<pre>\n")
            f.write(f"Ref: {ref_html}<br>\n")
            f.write(f"Seq: {test_html}<br>\n</pre><hr>\n")

        f.write("</body></html>\n")

def main():
    parser = argparse.ArgumentParser(description="MSA with Clustal Omega, group by alignment, output original sequences with merged headers.")
    parser.add_argument("-i", "--input", required=True, help="Input FASTA (unaligned)")
    parser.add_argument("-o", "--output-dir", default="msa_output", help="Output directory")
    parser.add_argument("-p", "--prefix", default="variants", help="Prefix for output files")
    parser.add_argument("--html", action="store_true", help="Generate HTML alignment report (from aligned sequences)")
    args = parser.parse_args()

    os.makedirs(args.output_dir, exist_ok=True)

    # Step 1: Run Clustal Omega
    aligned_path = os.path.join(args.output_dir, f"{args.prefix}_aligned.fasta")
    run_clustalo(args.input, aligned_path)

    # Step 2: Read aligned and original FASTA
    aligned_records = read_fasta(aligned_path)
    original_records = {rec.id: str(rec.seq) for rec in read_fasta(args.input)}

    if not aligned_records:
        print("❌ No sequences found after alignment.")
        return

    # Step 3: Group aligned sequences
    grouped = group_variants(aligned_records)  # [(merged_header, aligned_seq)]

    # Step 4: For each group, pick one original (unaligned) representative
    to_write = []
    for merged_header, _ in grouped:
        ids = merged_header.replace("variant|", "").split("|")
        for id_ in ids:
            if id_ in original_records:
                to_write.append((merged_header, original_records[id_]))
                break  # take first original match
        else:
            print(f"⚠️ No original sequence found for: {merged_header}")

    # Step 5: Write original sequences with merged headers
    unique_fasta = os.path.join(args.output_dir, f"{args.prefix}_unique.fasta")
    write_fasta(to_write, unique_fasta)
    print(f"✅ Original sequences with grouped headers saved to: {unique_fasta}")

    # Step 6: Optionally generate HTML from aligned sequences
    if args.html:
        html_path = os.path.join(args.output_dir, f"{args.prefix}_alignment.html")
        generate_html(grouped, html_path)
        print(f"✅ HTML alignment report saved to: {html_path}")

if __name__ == "__main__":
    main()

# Example code for run
# python align_and_group_clustalo.py -i Cep_gene.fasta -o Cep -p Cep --html