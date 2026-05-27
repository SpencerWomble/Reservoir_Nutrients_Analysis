
"""DataFrame plotting utilities for water quality data."""

import pandas as pd
import matplotlib.pyplot as plt

def pivot_wider(
    df: pd.DataFrame,
    columns=('characteristic', 'sample_fraction'),
    values: str = ['measure_value','measure_value_half_dl'],
    id_cols=None,
) -> pd.DataFrame:
    """Pivot a DataFrame wider for easier plotting.

    Parameters
    ----------
    df : pd.DataFrame
        Input data in long format.
    columns : str or sequence of str
        Column(s) whose unique values become new column headers.
    values : str
        Column whose values fill the new columns.
    id_cols : sequence of str, optional
        Columns that uniquely identify a row in the wide output.
        If None, defaults to date_time + site identifiers.
    """
    df = df.copy()
    columns = [columns] if isinstance(columns, str) else list(columns)

    if id_cols is None:
        id_cols = ['monitoring_location_identifier','date_time', 'site', 'id', 'position', 'year', 'month', 'decimal_date', 'latitude', 'longitude',
                   'distance_from_centerline_km', 'total_distance', "activity_depth_value"]
        id_cols = [c for c in id_cols if c in df.columns]

    wide = df.pivot_table(
        index=id_cols,
        columns=columns,
        values=values,
        aggfunc='first',
    ).reset_index()

    # Flatten the MultiIndex columns: ('Phosphorus', 'Total') -> 'Phosphorus_Total'
    wide.columns = [
        '_'.join(str(c) for c in col if c != '').strip('_') if isinstance(col, tuple) else col
        for col in wide.columns
    ]
    return wide



# plotting id locations by site and year
def plot_id(
    data: pd.DataFrame,
    var: str,
    site_col: str,
    site_name,
    x_col: str,
    y_col: str,
    id_col: str = 'id',
    kind: str = 'scatter',
) -> None:
    """Plot x_col against y_col faceted by site ID.

    Parameters
    ----------
    data : pd.DataFrame
        Input DataFrame.
    var : str
        Column to check for NA values. Rows where this is null are excluded.
    site_col : str
        Name of the column containing site identifiers.
    site_name : str or list
        One or more site values to filter to. A single string is wrapped automatically.
    x_col : str
        Column to plot on the x-axis.
    y_col : str
        Column to plot on the y-axis.
    id_col : str, default 'id'
        Column used for faceting.
    kind : {'scatter', 'bar', 'line'}, default 'scatter'
        Plot type.
    """
    if isinstance(site_name, str):
        site_name = [site_name]

    mask = data[site_col].isin(site_name) & data[var].notna()
    plot_df = data.loc[mask].copy()

    ids = plot_df[id_col].unique()
    if len(ids) == 0:
        print("No data after filtering.")
        return

    n_cols = 3
    n_rows = -(-len(ids) // n_cols)  # ceiling division

    fig, axes = plt.subplots(
        n_rows, n_cols,
        figsize=(n_cols * 4, n_rows * 3),
        sharey=False,
        squeeze=False,
    )
    axes = axes.flatten()

    last_used = -1
    for i, site_id in enumerate(ids):
        ax = axes[i]
        subset = plot_df[plot_df[id_col] == site_id].sort_values(x_col)
        if kind == 'bar':
            ax.bar(subset[x_col], subset[y_col])
        elif kind == 'line':
            ax.plot(subset[x_col], subset[y_col], marker='o')
        else:
            ax.scatter(subset[x_col], subset[y_col])
        ax.set_title(site_id)
        ax.set_xlabel(x_col)
        ax.set_ylabel(y_col)
        ax.tick_params(axis='x', rotation=45)
        ax.spines[['top', 'right']].set_visible(False)
        last_used = i

    for j in range(last_used + 1, len(axes)):
        axes[j].set_visible(False)

    plt.tight_layout()
    plt.show()