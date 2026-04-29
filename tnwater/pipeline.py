"""Top-level data loading and merging functions."""
from typing import Optional
 
import pandas as pd
 
from .preprocessing import (
    add_date_features,
    build_datetime,
    clean_column_names,
    select_columns,
)


def load_water_quality(
    path: str, columns: Optional[list] = None
) -> pd.DataFrame:
    """Load and preprocess the Tennessee water quality CSV.

    Steps
    -----
    1. Read the CSV.
    2. Clean column names (lowercase, underscores).
    3. Subset to the columns of interest.
    4. Build a combined datetime column from start_date + start_time.
    5. Convert timestamps to UTC using each row's timezone code.
    6. Add year, month, and decimal_date columns.
    """
    df = pd.read_csv(path)
    df = clean_column_names(df)
    df = select_columns(df, columns)
    df = build_datetime(df)
    df = add_date_features(df)
    return df
 
 
def load_gps(path: str) -> pd.DataFrame:
    """Load the dam distances / GPS reference table."""
    return pd.read_csv(path)
 
 
def merge_water_quality_with_gps(
    nut_df: pd.DataFrame,
    gps_df: pd.DataFrame,
    on: Optional[list] = None,
    how: str = "left",
) -> pd.DataFrame:
    """Join water quality data with GPS data on latitude/longitude."""
    if on is None:
        on = ["latitude", "longitude"]
    return pd.merge(nut_df, gps_df, on=on, how=how)
 
 
if __name__ == "__main__":
    nut_df = load_water_quality("data/wq_data_for_tennessee.csv")
    gps_df = load_gps("data/dam_distances.csv")
    merged_df = merge_water_quality_with_gps(nut_df, gps_df)
    print(merged_df.shape)
    print(merged_df.head())
 
