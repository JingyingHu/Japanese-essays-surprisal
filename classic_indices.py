import requests
import pandas as pd

# get dictonary data
file_path = ''
vocablist = pd.read_csv(file_path + 'goi1.1.csv', index_col = 0, encoding='shift_jis') #downloaded from Sunakawa et al., 2012 (https://jhlee.sakura.ne.jp/JEV/)

wordlist = []
worddic = {}
file = open("goi1.1.csv", encoding='shift_jis', errors = "ignore").read()
info = file.split("\n")[1:]  # skip header
for x in info:
    if x.strip():
        devided = x.split(",")
        if len(devided) > 6:
            worddic[devided[1]] = {
                'reading': devided[2],
                'vocab_level': devided[3],
                'POS': devided[4],
                'POS(detailed)': devided[5],
                'type': devided[6]
            }
wordlist = [worddic]

#get essay data
file_path = ''
essaydata = pd.read_csv(file_path + 'file_name.csv', index_col = 0, encoding='utf-8-sig')
essaydata

#analyze essays
# Calculate statistics for all rows in essaydata and add as columns

def analyze_text(text):
    # Handle missing or NaN values
    if pd.isna(text) or not isinstance(text, str):
        return pd.Series({
            'num_sentences': 0,
            'num_tokens': 0,
            'mean_tokens_per_sentence': 0,
            'type_token_ratio': 0,
            'num_verbs': 0,
            'total_char_count': 0,
            'LIM_count': 0,
            'HIM_count': 0,
            'wago_count': 0,
            'kango_count': 0,
        })
    
    # Remove hyphens for consistency
    text = text.replace("-", "")
    # Get analysis from MeCab API
    url = ""
    payload = {'text': text}
    response = requests.post(url, data=payload)
    response.encoding = 'utf-8'
    lines = response.text.split("\n")
    
    # Sentence count
    def sentence_count(lines):
        sentence_num = 0
        for x in lines:
            if x == "EOS":
                continue
            result = x.split("\t")
            if len(result) > 0 and (("。" in result[0]) or ("." in result[0]) or ("．" in result[0]) or ("！" in result[0]) or ("!" in result[0]) or ("?" in result[0]) or ("？" in result[0])):
                sentence_num += 1
        return(sentence_num)

    # Tokenize
    def tokenize(lines):
        tokenized = []
        for x in lines:
            if x == "EOS":
                continue
            result = x.split("\t")
            if len(result) < 4 or ("記号" in result[3]) or ("-" in result[0]):
                continue
            tokenized.append(result[2])
        return tokenized    

    # Lemmatize
    def lemmatize(lines):
        lemmatized = []
        for x in lines:
            if x == "EOS":
                continue
            result = x.split("\t")
            if len(result) < 4 or ("助詞" in result[3]) or ("記号" in result[3]) or ("-" in result[0]):
                continue
            lemmatized.append(result[2])
        return lemmatized

    # Verb count
    def verb_count(lines):
        verblist = []
        for x in lines:
            if x == "EOS":
                continue
            result = x.split("\t")
            # Check if result has enough fields and if POS (result[3]) starts with '動詞'
            if len(result) > 3 and result[3].startswith("動詞"):
                verblist.append(result[2])  # Assuming the verb is in the 3rd column (index 2)
        return len(set(verblist))

    # Lookup dictionary
    def lookupdic(dic, key):
        for d in dic:
            if key in d:
                return d[key]
        return False

    # Check level
    def check_level(lemmas):
        checked = {}
        for lemma in lemmas:
            info = lookupdic(wordlist, lemma)
            if info:
                level = info['vocab_level']
            else:
                level = 'undefined'
            if level in checked:
                checked[level].append(lemma)
            else:
                checked[level] = [lemma]
        return checked

    # Check type
    def check_type(lemmas):
        checked = {}
        for lemma in lemmas:
            info = lookupdic(wordlist, lemma)
            if info:
                level = info['type']
            else:
                level = 'undefined'
            if level in checked:
                checked[level].append(lemma)
            else:
                checked[level] = [lemma]
        return checked

    # Start analysis
    num_sentences = sentence_count(lines)
    tokenes = tokenize(lines)
    num_tokens = len(tokenes)
    lemmas = lemmatize(lines)
    type_token_ratio = len(set(tokenes)) / num_tokens if num_tokens > 0 else 0
    mean_tokens_per_sentence = num_tokens / num_sentences if num_sentences > 0 else 0
    num_verbs = verb_count(lines)
    total_char_count = len(text)
    result_level = check_level(lemmas)
    result_type = check_type(lemmas)
    LIM_count = len(result_level.get('3.中級前半', []))
    HIM_count = len(result_level.get('4.中級後半', []))
    wago_count = len(result_type.get('和語', []))
    kango_count = len(result_type.get('漢語', []))

    return pd.Series({
        'mean_tokens_per_sentence': mean_tokens_per_sentence,
        'type_token_ratio': type_token_ratio,
        'num_verbs': num_verbs,
        'total_char_count': total_char_count,
        'LIM_count': LIM_count,
        'HIM_count': HIM_count,
        'wago_count': wago_count,
        'kango_count': kango_count,
    })

# Apply to all rows
essay_stats = essaydata['text'].apply(analyze_text)
essaydata_with_stats = pd.concat([essaydata, essay_stats], axis=1)

# Export to CSV
essaydata_with_stats.to_csv(file_path + 'file_name_classic_indices.csv', encoding='utf-8-sig')
