#!/bin/bash

# Directory containing the CSV files
csv_dir="/Volumes/Transcend/2024_BFS_Sequence_data_for_phylogeny/Sequence_data/variant_call/SNP_call_snippy/SNP_folder"

# Output file
output_file="merged.csv"

# Navigate to the directory
cd "$csv_dir" || exit 1

# Check if any CSV files exist
if ls *.csv &>/dev/null; then
    # Add the header from the first file to the output file
    head -n 1 "$(ls *.csv | head -n 1)" > "$output_file"

    # Append data from all files, skipping headers
    for file in *.csv; do
        tail -n +2 "$file" >> "$output_file"
    done

    echo "All CSV files merged into $output_file"
else
    echo "No CSV files found in $csv_dir"
fi
