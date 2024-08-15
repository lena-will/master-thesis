# Housekeeping ---------------------------------------------------------------------------------------------------------

from datetime import date, datetime, timedelta
from dateutil import relativedelta
import pandas as pd
import typing as t

# Load data ------------------------------------------------------------------------------------------------------------

ip_index = pd.read_csv('ip_index.csv', index_col=0)
ip_index['availability'] = pd.to_datetime(ip_index['availability'])

cpi = pd.read_csv('cpi.csv', index_col=0)
cpi['Date'] = pd.to_datetime(cpi['Date'])

term_spread = pd.read_csv('term_spread.csv', index_col=0)
term_spread['availability'] = pd.to_datetime(term_spread['availability'])

vacancies = pd.read_csv('vacancies.csv', index_col=0)
vacancies['Date'] = pd.to_datetime(vacancies['Date'])

esi = pd.read_csv('esi.csv', index_col=0)
esi['Date'] = pd.to_datetime(esi['Date'])

text_indicator = pd.read_csv('text_indicator.csv', index_col=0)
text_indicator['Datum'] = pd.to_datetime(text_indicator['Datum'])

# Function to calculate week of quarter based on date ------------------------------------------------------------------

next_monday = relativedelta.relativedelta(weekday=relativedelta.MO)
previous_monday = relativedelta.relativedelta(weekday=relativedelta.MO(-1))
one_week = timedelta(weeks=1)


def week_in_quarter(dt: datetime) -> t.Tuple[int, int]:
    d: date = dt.date()
    year = d.year

    # Q1 = January 1, Q2 = April 1, Q3 = July 1, Q4 = October 1
    quarter = ((d.month - 1) // 3)
    quarter_start = date(year, (quarter * 3) + 1, 1)
    quarter_week_2_monday = quarter_start + next_monday

    if d < quarter_week_2_monday:
        week = 1
    else:
        cur_week_monday = d + previous_monday
        week = int((cur_week_monday - quarter_week_2_monday) / one_week) + 1

    return quarter, week

# find week of quarter for macro data ----------------------------------------------------------------------------------

ip_index["quarter_avail"] = ip_index["availability"].apply(lambda availability: week_in_quarter(availability)[0]+1)
ip_index["week_avail"] = ip_index["availability"].apply(lambda availability: week_in_quarter(availability)[1])

cpi["quarter_avail"] = cpi["Date"].apply(lambda date: week_in_quarter(date)[0]+1)
cpi["week_avail"] = cpi["Date"].apply(lambda date: week_in_quarter(date)[1])

term_spread["quarter_avail"] = term_spread["availability"].apply(lambda availability: week_in_quarter(availability)[0]+1)
term_spread["week_avail"] = term_spread["availability"].apply(lambda availability: week_in_quarter(availability)[1])

vacancies["quarter_avail"] = vacancies["Date"].apply(lambda date: week_in_quarter(date)[0]+1)
vacancies["week_avail"] = vacancies["Date"].apply(lambda date: week_in_quarter(date)[1])

esi["quarter_avail"] = esi["Date"].apply(lambda date: week_in_quarter(date)[0]+1)
esi["week_avail"] = esi["Date"].apply(lambda date: week_in_quarter(date)[1])

text_indicator["quarter_avail"] = text_indicator["Datum"].apply(lambda date: week_in_quarter(date)[0]+1)
text_indicator["week_avail"] = text_indicator["Datum"].apply(lambda date: week_in_quarter(date)[1])

# Adjust for years in which first week ofo the year starts on Monday of previous year ----------------------------------
adjust_year = {2002, 2003, 2004, 2008, 2009, 2013, 2014, 2015, 2019, 2020}

cpi["week_avail"] = cpi.apply(lambda row: row["week_avail"] + int(row["Date"].year in adjust_year), axis = 1)
cpi["quarter_avail"] = cpi.apply(lambda row: row["quarter_avail"] + int(row["week_avail"] == 14), axis = 1)
cpi["week_avail"] = cpi["week_avail"].apply(lambda week: week % 13)

ip_index["week_avail"] = ip_index.apply(lambda row: row["week_avail"] + int(row["availability"].year in adjust_year), axis = 1)
ip_index["quarter_avail"] = ip_index.apply(lambda row: row["quarter_avail"] + int(row["week_avail"] == 14), axis = 1)
ip_index["week_avail"] = ip_index["week_avail"].apply(lambda week: week % 13)

term_spread["week_avail"] = term_spread.apply(lambda row: row["week_avail"] + int(row["availability"].year in adjust_year), axis = 1)
term_spread["quarter_avail"] = term_spread.apply(lambda row: row["quarter_avail"] + int(row["week_avail"] == 14), axis = 1)
term_spread["week_avail"] = term_spread["week_avail"].apply(lambda week: week % 13)

esi["week_avail"] = esi.apply(lambda row: row["week_avail"] + int(row["Date"].year in adjust_year), axis = 1)
esi["quarter_avail"] = esi.apply(lambda row: row["quarter_avail"] + int(row["week_avail"] == 14), axis = 1)
esi["week_avail"] = esi["week_avail"].apply(lambda week: week % 13)

text_indicator["week_avail"] = text_indicator.apply(lambda row: row["week_avail"] + int(row["Datum"].year in adjust_year), axis = 1)
text_indicator["quarter_avail"] = text_indicator.apply(lambda row: row["quarter_avail"] + int(row["week_avail"] == 14), axis = 1)
text_indicator["week_avail"] = text_indicator["week_avail"].apply(lambda week: week % 13)

vacancies["week_avail"] = vacancies.apply(lambda row: row["week_avail"] + int(row["Date"].year in adjust_year), axis = 1)
vacancies["quarter_avail"] = vacancies.apply(lambda row: row["quarter_avail"] + int(row["week_avail"] == 14), axis = 1)
vacancies["week_avail"] = vacancies["week_avail"].apply(lambda week: week % 13)

ip_index.to_csv("/Users/lena/PycharmProjects/MasterThesis/ip_index_mapping.csv", encoding="utf-8-sig")
cpi.to_csv("/Users/lena/PycharmProjects/MasterThesis/cpi_mapping.csv", encoding="utf-8-sig")
term_spread.to_csv("/Users/lena/PycharmProjects/MasterThesis/term_spread_mapping.csv", encoding="utf-8-sig")
vacancies.to_csv("/Users/lena/PycharmProjects/MasterThesis/vacancies_mapping.csv", encoding="utf-8-sig")
esi.to_csv("/Users/lena/PycharmProjects/MasterThesis/esi_mapping.csv", encoding="utf-8-sig")
text_indicator.to_csv("/Users/lena/PycharmProjects/MasterThesis/text_indicator_mapping.csv", encoding="utf-8-sig")
