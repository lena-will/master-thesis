# Housekeeping ---------------------------------------------------------------------------------------------------------
import argparse
import os
from pathlib import Path
import nltk
import pandas as pd
from bs4 import BeautifulSoup


nltk.download('punkt')
nltk.download('stopwords')


# Import xml data and format data by "Artikel" -------------------------------------------------------------------------
def main(folder_name):
    path = Path(f"/Users/lena/Desktop/faz_data/utf_8/{folder_name}")
    list_files = os.listdir(path)
    files = [file for file in list_files if
             file.endswith(".xml")]  # drop .zip files and DS Store files for Mac Spotlight search

    faz_data = []

    for file in files:
        print(path / file)
        with open(path / file, "r") as f:
            data = f.read()

        xml_file = BeautifulSoup(data, "xml")
        xml_artikel = xml_file.find_all("artikel")
        artikel_text_mapping = {}

        for artikel in xml_artikel:
            artikel_id = artikel.find("artikel-id").text
            artikel_text_mapping[artikel_id] = ". ".join([absatz.text for absatz in artikel.find_all("absatz")])
        artikel_df = pd.DataFrame.from_dict(artikel_text_mapping, orient='index')

        datum_ini = {}
        for artikel in xml_artikel:
            artikel_id = artikel.find("artikel-id").text
            datum_ini[artikel_id] = "".join([datum.text for datum in artikel.find_all("datum")])
        datum_df = pd.DataFrame.from_dict(datum_ini, orient='index')

        dfs = [artikel_df, datum_df]
        artikel_tmp = pd.concat(dfs, axis=1)
        faz_data.append(artikel_tmp)

    faz_data_df = pd.concat(faz_data, axis=0)
    column_names = ["Artikel", "Datum"]
    faz_data_df.columns = column_names
    faz_data_df['Datum'] = pd.to_datetime(faz_data_df['Datum'], format='%d%m%Y')
    faz_data_df = faz_data_df.sort_values(by='Datum')
    # subset = faz_data_df.head(5).copy()

    # Pre-processing -------------------------------------------------------------------------------------------------------
    # tokenise by sentences
    faz_data_df["Artikel"] = faz_data_df["Artikel"].apply(lambda text: nltk.sent_tokenize(text, language="german"))
    # lowercase all words
    faz_data_df["Artikel"] = faz_data_df["Artikel"].apply(lambda text: [str.lower(w) for w in text])
    # keyword indicator
    keywords = {"arbeitslosenquote", "arbeitslosigkeit"}
    keyword_indicator = faz_data_df.copy()
    keyword_indicator["key_sentences"] = keyword_indicator["Artikel"].apply(
        lambda sentences: [s for s in sentences if any(keyword in s for keyword in keywords)])
    keyword_indicator = keyword_indicator[keyword_indicator["key_sentences"].map(len) > 0]

    keyword_indicator["key_sentences"] = keyword_indicator["key_sentences"].apply(lambda sentences: " ".join(sentences))
    keyword_indicator["key_sentences_tokens"] = keyword_indicator["key_sentences"].apply(lambda text: nltk.word_tokenize(text, language="german"))
    keyword_indicator["key_sentences_tokens"] = keyword_indicator["key_sentences_tokens"].apply(lambda text: [w for w in text if len(w) > 1])
    keyword_indicator["key_sentences_tokens"] = keyword_indicator["key_sentences_tokens"].apply(lambda text: [w for w in text if not w.isdigit()])
    keyword_indicator["key_sentences_tokens"] = keyword_indicator["key_sentences_tokens"].apply(lambda text: [w for w in text if not w.startswith('-')])
    keyword_indicator["key_sentences_tokens"] = keyword_indicator["key_sentences_tokens"].apply(lambda text: [w for w in text if not w.startswith("'")])
    keyword_indicator["key_sentences_tokens"] = keyword_indicator["key_sentences_tokens"].apply(lambda text: [w for w in text if not w.startswith(" ")])
    keyword_indicator["key_sentences_tokens"] = keyword_indicator["key_sentences_tokens"].apply(lambda text: [w for w in text if not w.startswith("``")])
    letters = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "ä", "ö", "ü", "ß"]
    keyword_indicator["key_sentences_tokens"] = keyword_indicator["key_sentences_tokens"].apply(lambda text: [w for w in text if any(ele in w for ele in letters)])
    keyword_indicator["key_sentences_string"] = keyword_indicator["key_sentences_tokens"].apply(lambda text: " ".join(text))
    # save to csv for analysis in R
    keyword_indicator.to_csv(path / "keyword_unemp.csv", encoding="utf-8-sig")


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--folder", type=str, help="Folder containing faz_data")
    args = parser.parse_args()
    print(f"Running pre-processing for {args.folder} ...")
    main(args.folder)
    print("Done!")
