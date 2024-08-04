# Housekeeping ---------------------------------------------------------------------------------------------------------
import argparse
import os
import re
from pathlib import Path

import nltk
import numpy as np
import pandas as pd
from HanTa import HanoverTagger as ht
from bs4 import BeautifulSoup
from nltk.corpus import stopwords
from sklearn.feature_extraction.text import TfidfVectorizer
from spacy.lang.de.stop_words import STOP_WORDS

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

    # remove punctuation
    punctuation = '.,;!?/``´´+*#^"§$%&/()=@:·¸¡°'
    punct = set(punctuation)
    faz_data_df["Artikel"] = faz_data_df["Artikel"].apply(lambda text: ''.join(w for w in text if w not in punct))

    # tokenizing
    faz_data_df["Artikel"] = faz_data_df["Artikel"].apply(lambda text: nltk.word_tokenize(text, language="german"))
    faz_data_df["Tokens"] = faz_data_df["Artikel"]
    faz_data_df["Tokens_string"] = faz_data_df["Artikel"].apply(lambda text: ' '.join(w for w in text))

    # digits and one-character tokens
    faz_data_df["Artikel"] = faz_data_df["Artikel"].apply(lambda text: [w for w in text if len(w) > 1])
    faz_data_df["Artikel"] = faz_data_df["Artikel"].apply(lambda text: [w for w in text if not w.isdigit()])
    faz_data_df["Artikel"] = faz_data_df["Artikel"].apply(lambda text: [w for w in text if not w.startswith('-')])
    faz_data_df["Artikel"] = faz_data_df["Artikel"].apply(lambda text: [w for w in text if not w.startswith("'")])

    # remove tokens which do not contain any letters
    letters = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "ä", "ö", "ü", "ß"]
    faz_data_df["Artikel"] = faz_data_df["Artikel"].apply(lambda text: [w for w in text if any(ele in w for ele in letters)])

    # remove stopwords
    stopwords_de = set(stopwords.words("german"))  # 232 stopwords
    stopwords_spacy = STOP_WORDS
    inAnotB = stopwords_de.difference(stopwords_spacy)
    stopwords_spacy.update(inAnotB)  # 567 stopwords
    faz_data_df["Artikel"] = faz_data_df["Artikel"].apply(
        lambda text: [w for w in text if not w.lower() in stopwords_spacy])

    # remove common first names
    name_list = pd.read_csv('first_names.csv', delimiter=';')
    name_list = name_list.apply(lambda name: name.str.lower())
    unique_name_list = set(name_list['name'])
    faz_data_df["Artikel"] = faz_data_df["Artikel"].apply(
        lambda text: [w for w in text if not w.lower() in unique_name_list])

    # lemmatisation
    tagger = ht.HanoverTagger('morphmodel_ger.pgz')
    faz_data_df["Stemming"] = faz_data_df["Artikel"]
    faz_data_df["Stemming"] = faz_data_df["Stemming"].apply(lambda text: tagger.tag_sent(text))
    faz_data_df["Stemming"] = faz_data_df["Stemming"].apply(
        lambda text: [w[1] for w in text])  # use first element of results which is the stemmed word

    # lower case
    faz_data_df["Stemming"] = faz_data_df["Stemming"].apply(lambda text: [str.lower(w) for w in text])

    # prep final df for export
    faz_data_df["Tokens_after"] = faz_data_df["Stemming"]
    faz_data_df["Stemming"] = faz_data_df["Stemming"].apply(lambda text: ' '.join(w for w in text))
    #faz_data_df = faz_data_df.drop("tf_idf", axis=1)

    # save to csv for analysis in R
    faz_data_df.to_csv(path / "artikel_df.csv", encoding="utf-8-sig")


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--folder", type=str, help="Folder containing faz_data")
    args = parser.parse_args()
    print(f"Running pre-processing for {args.folder} ...")
    main(args.folder)
    print("Done!")
