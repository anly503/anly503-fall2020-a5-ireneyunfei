import altair as alt
import pandas as pd

## from 10-min granularity to one hour
df = pd.read_csv("05_sm2.csv")

df2 = df.groupby(['date']).sum()

df2.reset_index(inplace=True)
df2 = df2.rename(columns = {'index':'date'})

df2['date']= pd.to_datetime(df2['date'])
df2['month'] = pd.DatetimeIndex(df2['date']).month
df2['month_s'] = df2.month.astype(str)


## define brush
brush = alt.selection(type='interval', resolve='global')

## left chart - relationship of powerl1 & powerl2
chart1 = alt.Chart(df2).mark_point().encode(
    alt.X('powerl2', type='quantitative'),
    alt.Y('powerl1', type='quantitative'),
    color=alt.condition(brush, 'month:N', alt.ColorValue('gray')),
    order =alt.Order('month',sort='descending')
).add_selection(
    brush
).properties(
    width=250,
    height=250
)

## left chart - relationship of powerl1 & powerl3
chart2 = alt.Chart(df2).mark_point().encode(
    alt.X('powerl3', type='quantitative'),
    alt.Y('powerl1', type='quantitative'),
    color=alt.condition(brush, 'month:N', alt.ColorValue('gray')),
    order =alt.Order('month',sort='descending')
).add_selection(
    brush
).properties(
    width=250,
    height=250
)
a = (chart1|chart2).properties(title = 'Relationship among Power Phases by month (Household#5)').configure_title(fontSize=24,align = 'center')

## save charts
a.save('altair.html')