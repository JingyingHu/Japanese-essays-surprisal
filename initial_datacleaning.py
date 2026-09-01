import csv
import re
import re

def clean(txt):
    if not txt:
        return ""
    
    # 1. Standard character clean-up
    tmp = txt.replace(',', '、').replace('.', '。')
    tmp = txt.replace('，', '、').replace('．', '。')
    tmp = tmp.replace('!', '！').replace('?', '？')
    # Use a set of characters to remove to keep code clean
    for char in ['-', ' ', '　', '\t', '【名前】']:
        tmp = tmp.replace(char, '')
    
    # 2. Remove character counts
    tmp = re.sub(r'（\d+字）', '', tmp)

    # 3. Smart Title Removal
    lines = tmp.splitlines()
    
    # Find the first line that actually contains text
    first_text_index = -1
    for i, line in enumerate(lines):
        if line.strip():
            first_text_index = i
            break
            
    if first_text_index != -1:
        first_line_content = lines[first_text_index].strip()
        
        # Check title criteria: shorter than 26 characters
        if len(first_line_content) < 26:
            # It's a title! Skip it and keep everything after.
            tmp = "\n".join(lines[first_text_index + 1:])
        else:
            # It's likely a sentence! Keep it and everything after.
            tmp = "\n".join(lines[first_text_index:])
    else:
        tmp = ""
        
    return tmp.strip()

# ------- MAIN ---------

input_file_name = 'file_name.csv'
output_file_name = 'file_name_processed.csv'
rows = []

# Read all data into memory
with open(input_file_name, mode='r', encoding='utf-8-sig', newline='') as f:
    reader = csv.DictReader(f)
    fieldnames = reader.fieldnames

# clean essays
    for row in reader:
        cleaned_txt = clean(row['text'])
        row['text'] = cleaned_txt
        rows.append(row)

# Write data back (overwriting the file)
with open(output_file_name, mode='w', encoding='utf-8-sig', newline='') as f:
    writer = csv.DictWriter(f, fieldnames=fieldnames)
    writer.writeheader()
    c = 0
    for row in rows:
        writer.writerow(row)
        c += 1

    print(f"Lines written: {c + 1}")