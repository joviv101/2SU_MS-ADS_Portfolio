#2019-1002 IST 718 Group 2
#Andrew Miller, Jo Vivian, Michael Morales, David Molenda
#Application to explore propery trends

#Import the required packages
from bokeh.models import Select, ColumnDataSource, HoverTool, Div
from bokeh.io import curdoc
from bokeh.layouts import row, layout, column
from bokeh.plotting import figure
import pandas as pd

#Set up the default selection
PropertySelect = 'Property: 35 West Wacker'

#Bring in the Predicted time series data
#TODO: Use your path instead of this path: /home/dm/Documents/DocumentsDS5/IST 718/Project/
dfCost = pd.read_csv('/home/dm/Documents/DocumentsDS5/IST 718/Project/costoutput.csv')

#Convert the date column to a datetime object and extract readable Data.
dfCost['ds'] = pd.to_datetime(dfCost['ds'])
dfCost['Date2'] = pd.to_datetime(dfCost['ds']).apply(lambda x:x.strftime('%Y-%m-%d'))

#Select the default property
Plotdf1 = dfCost[dfCost['Address'] == PropertySelect]

#Set up the data source for the time series chart
data_points = ColumnDataSource(data = {'x': Plotdf1['ds'], 'y': Plotdf1['yhat'], 'z' : Plotdf1['Date2'], 'h' : Plotdf1['yhat_upper'], 'l' : Plotdf1['yhat_lower']})

#import the HTML header for the application
desc = Div(text=open("/home/dm/Documents/DocumentsDS5/IST 718/Project/AppTitle.html").read(), width=600, height=200)

#Define the Hover Tool display values
hover_tool = HoverTool(tooltips = [
    ('Date', '@z'),
    ('Value', '@y'),
    ('Upper', '@h'),
    ('Lower', '@l')
    ])

#Create the time series chart
p1 = figure(x_axis_type="datetime", title="Cost", tools = [hover_tool, 'box_zoom,reset' ], plot_width=1217, plot_height=500)
p1.grid.grid_line_alpha=0.3
p1.xaxis.axis_label = 'Date'
p1.yaxis.axis_label = 'Unit Cost'

#Add the lines to the time series chart
p1.line(x = 'x', y = 'y', source = data_points, color='#A6CEE3', line_width=4)
p1.line(x = 'x', y = 'h', source = data_points, color='#A2FADE', line_width=2)
p1.line(x = 'x', y = 'l', source = data_points, color='#A2FADE', line_width=2)

#Create the select widget from a list of the properties
AllProperties = dfCost['Address'].unique()
AllProperties = AllProperties.tolist()
select_widget = Select(options = AllProperties, value = PropertySelect, title = 'Select the Address of interest:')


#Define the callback function update when a new property is selected
def callback(attr, old, new):
    PropertySelect = select_widget.value
    Plotdf1 = dfCost[dfCost['Address'] == PropertySelect]
    data_points.data = {'x': Plotdf1['ds'], 'y': Plotdf1['yhat'], 'z' : Plotdf1['Date2'], 'h' : Plotdf1['yhat_upper'], 'l' : Plotdf1['yhat_lower']}
     
    
#When selection changes, update the data
select_widget.on_change('value', callback)

#Create a layout for the application
charts = [p1]
outputs = row(*charts, width=950, height=800)
layout = layout([desc], [select_widget],  [outputs])

#Add the layout to the application
curdoc().add_root(layout)
curdoc().title = "Property Browser"

#Launching the application
#bokeh serve --show bokehSelection2.py


