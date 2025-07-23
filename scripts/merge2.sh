#!/bin/bash

# Directory containing the CSV files
csv_dir="/Volumes/Transcend/2024_BFS_Sequence_data_for_phylogeny/Sequence_data/variant_call/SNP_call_snippy/SNP_folder"

# Output file
output_file="merged.txt"

# Navigate to the directory
cd "$csv_dir" || exit 1

# Check if any CSV files exist
if ls *.csv &>/dev/null; then
    # Add the header from the first file to the output file, prefixed with "Filename"
    echo "Filename,$(head -n 1 "$(ls *.csv | head -n 1)")" > "$output_file"

    # Append data from all files, skipping headers and adding the filename
    for file in *.csv; do
        tail -n +2 "$file" | awk -v fname="$file" '{print fname "," $0}' >> "$output_file"
    done

    echo "All CSV files merged into $output_file with filenames prepended."
else
    echo "No CSV files found in $csv_dir"
fi
