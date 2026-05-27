"""DataFrame preprocessing utilities for water quality data."""
from typing import Optional
 
import pandas as pd
import numpy as np


 

# Default columns to retain from the raw WQX export
DEFAULT_COLUMNS = [
    'monitoring_location_identifier',
    "start_date",
    "start_time",
    "characteristic",
    "measure_value",
    "measure_unit_code",
    "sample_fraction",
    "quantitation_limit_value",
    "analytical_method_identifier",
    "result_speciation",
    "latitude",
    "longitude",
    "huc_eight_name",
    "start_time_zone_code",
    "activity_depth_value",
    "activity_depth_unit_code"
]

"""Project-wide constants for Tennessee water quality processing."""

# Default set of site IDs with good data coverage
DEFAULT_SITE_IDS = ['pp16', 'cent_h13',      'pp8',  'cent_h1',  'cent_h7',
      'pp6',     'pp11', 'cent_h16',     'dh17',     'dh16',      'dh8',
      'dh6', 'cent_h14',  'cent_h9', 'cent_h12',  'cent_h6',  'cent_h2',
  'cent_h8',  'cent_h5',  'cent_h3',     'pp15',     'pp12',      'pp4',
      'pp3',      'pp2',      'pp1',      'pp9',     'pp10',     'pp13',
     'pp14',      'pp7',     'dh10',     'dh15',      'dh4',      'dh2',
      'dh9',     'dh11',      'dh3',     'dh14',     'dh13',     'dh18',
     'dh12',      'dh1',      'dh5', 'cent_h20',  'cent_h4', 'cent_h11',
      'pp5', 'cent_h19', 'cent_h17', 'cent_h18', 'cent_h15', 'cent_h10',
      'dh7']
 



def clean_column_names(df: pd.DataFrame) -> pd.DataFrame:
    """Lowercase column names and replace spaces with underscores."""
    df = df.copy()
    df.columns = df.columns.str.replace(" ", "_").str.lower()
    return df
 
 
def select_columns(
    df: pd.DataFrame, columns: Optional[list] = None
) -> pd.DataFrame:
    """Subset to a defined column list. Defaults to DEFAULT_COLUMNS."""
    if columns is None:
        columns = DEFAULT_COLUMNS
    return df.loc[:, columns]
 
 
def build_datetime(
    df: pd.DataFrame,
    date_col: str = "start_date",
    time_col: str = "start_time",
    output_col: str = "date_time",
) -> pd.DataFrame:
    """Combine separate date and time columns into a single datetime column."""
    df = df.copy()
    df[output_col] = pd.to_datetime(
        df[date_col].astype(str) + " " + df[time_col].astype(str)
    )
    return df
 


def filter_ids(
    df: pd.DataFrame,
    ids: Optional[list[str]] = None
) -> pd.DataFrame:
    """Filter rows to a set of site IDs.

    If no IDs are provided, uses DEFAULT_SITE_IDS.
    """
    if 'id' not in df.columns:
        raise ValueError("DataFrame must contain an 'id' column")

    if ids is None:
        ids = DEFAULT_SITE_IDS

    id_set = set(ids)
    return df[df['id'].isin(id_set)].copy() 


def _decimal_date(dt: pd.Series) -> pd.Series:
    """Convert a datetime Series to decimal years.
    Mirrors lubridate::decimal_date() in R. Leap years are handled
    using the actual length of each calendar year, including the
    century rule (years divisible by 100 but not 400 are not leap).
    Sub-day precision is preserved down to microseconds.

    Parameters
    ----------
    dt : array-like of datetimes
        Will be coerced via pd.to_datetime().

    Returns
    -------
    pd.Series
        Decimal year as float. NaT inputs become NaN.

    Examples
    --------
    >>> s = pd.to_datetime(pd.Series(['2023-01-01', '2024-07-01']))
    >>> _decimal_date(s)
    0    2023.000000
    1    2024.498634
    dtype: float64
    """
    dt = pd.to_datetime(dt)
    year = dt.dt.year
    doy = dt.dt.dayofyear
    seconds_in_day = (
        dt.dt.hour * 3600
        + dt.dt.minute * 60
        + dt.dt.second
        + dt.dt.microsecond / 1e6
    )
    elapsed = (doy - 1) * 86400 + seconds_in_day
    is_leap = ((year % 4 == 0) & (year % 100 != 0)) | (year % 400 == 0)
    days_in_year = 365 + is_leap.astype(float)
    return year + elapsed / (days_in_year * 86400)


def add_date_features(df: pd.DataFrame, dt_col: str = "date_time") -> pd.DataFrame:
    """Add year, month, and decimal date columns derived from a datetime column.

    Parameters
    ----------
    df : pd.DataFrame
        Input DataFrame containing a datetime column.
    dt_col : str
        Name of the datetime column. Defaults to 'date_time'.

    Returns
    -------
    pd.DataFrame
        Copy of df with 'year', 'month', and 'decimal_date' columns appended.
    """
    df = df.copy()
    df["year"] = df[dt_col].dt.year
    df["month"] = df[dt_col].dt.month
    df["decimal_date"] = _decimal_date(df[dt_col])
    return df


# feet to meters function for the depths
def ft_to_m(df: pd.DataFrame, depth_col: str = "activity_depth_value",
            unit_col: str = "activity_depth_unit_code") -> pd.DataFrame:
    """Convert feet to meters"""
    df[depth_col] = np.where(df[unit_col] == 'ft',
                             df[depth_col] / 3.281,
                             df[depth_col])
    return df


# quick outlier check function
def outlier_check(df, sort_col, keep_col, rows):
    """Check for outliers in first x many rows"""
    sub_df = (df
    .sort_values(by=sort_col, ascending=False)
    .head(rows)
    [[keep_col]]
    )
    return sub_df

# function to convert outliers to Nan - Note that this mutates in-place
def outlier_to_na(df, col, greater_than_threshold):
    df[col] = df[col].mask(df[col] > greater_than_threshold)
    return df

def remove_before_dash(df, column):
    """Remove everything before and including the last '-' in a string column."""
    df[column] = df[column].str.split('-').str[-1].str.strip()
    return df