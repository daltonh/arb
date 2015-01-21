#!/usr/bin/env python

# user interface for visualising contents of output/output_step.csv
# requires additional python libraries which can be installed following these steps


# ubuntu:

# sudo apt-get install python-pip 
# sudo apt-get install python-dev 
# sudo pip install --upgrade numpy 
# sudo pip install --upgrade pandas 
# sudo pip install --upgrade numexpr 
# sudo apt-get install python-wxgtk2.8 python-wxtools wx2.8-doc wx2.8-examples wx2.8-headers wx2.8-i18n
# sudo apt-get install python-matplotlib

# osx:

# easiest method is to use a python install such as 
# enthought canopy express (https://store.enthought.com/downloads/)


# import required modules
import time
import os
import sys
import wx
import  wx.lib.mixins.listctrl  as  listmix
import subprocess

from threading import Thread
from wx.lib.pubsub import Publisher

import pandas as pd
import numpy as np

import matplotlib
matplotlib.use('WXAgg')
from matplotlib.backends.backend_wxagg import FigureCanvasWxAgg as FigureCanvas
from matplotlib.figure import Figure

# flags for axis panels
# setup like this in case future expansion, eg. might want to add x2 axis
axis = {}
axis[0] = 'x1 axis'
axis[1] = 'y1 axis'
axis[2] = 'y2 axis'


# These lists store the colors that are cycled over when plotting
# The length of each list does not matter
# HEX color cyle for y1 axis
list1 = [
'#08088A',
'#8A0808',
'#6A0888',
'#4B8A08',
'#886A08',
'#424242',
'#8A0829',
'#088A68',
]

# HEX color cyle for y2 axis
list2 = [
'#5858FA',
'#FA5858',
'#D358F7',
'#82FA58',
'#FAAC58',
'#848484',
'#FA5882',
'#58FAAC',
]

# font size for legend
legend_font_size=11

class Data():
    def __init__(self):

        # read output_step.csv into pandas data frame
        # ignore the row of dimension strings ([1:])
        # (in future, could use dimension strings to automatically show dimensions in plot labels)
        
        self.step_file_path = 'output/output_step.csv'
        try:
            self.step_file = open(self.step_file_path)
        except:
            sys.exit('ERROR: output/output_step.csv does not exist') 

        self.df=pd.read_csv(self.step_file, comment='#')[1:]
        # convert all but header labels to float values
        self.df = self.df[1:].astype(float)
        self.step_file.close()

        self.df=pd.read_csv('output/output_step.csv', comment='#')[1:]
        # convert all but header labels to float values
        self.df = self.df[1:].astype(float)
                            
        # disctionaries needed for ColumnSorterMixin
        self.variables = {}
        self.variables_stripped = {}
        for i, variable in enumerate(self.df.columns):
            self.variables[i+1] = variable
            chars = ("'", '<', '>')
            for char in chars:
                variable = variable.replace(char,'')
            self.variables_stripped[i+1] = variable
        
        # store a reverse dictionary (possible as variable numbers are unique)
        # can use this to look up variable ID if variable name (eg "'<timestep>'") is known
        self.inverted = dict([[v,k] for k,v in self.variables.items()])

class RefreshThread(Thread):
    def __init__(self):
        self.refresh_state = True
        Thread.__init__(self)
        self.daemon = True # thread will die on window close
        self.start()    # start the thread
 
    def run(self):
        # This is the code executing in the new thread.
        while (self.refresh_state):
            time.sleep(2) # wait n seconds
            wx.CallAfter(self.postTime, 1)
        wx.CallAfter(Publisher().sendMessage, "update", "string") # send a string to the "update" broadcast (will allow thread to continue)
 
    def postTime(self, amt):
        Publisher().sendMessage("update", 1) # send an integer to the "update" stream (will stop thread)

class SortableListCtrl( wx.ListCtrl, listmix.ListCtrlAutoWidthMixin, listmix.CheckListCtrlMixin ) :

    def __init__( self, parent, ID, axis, pos=wx.DefaultPosition,
              size=wx.DefaultSize, style=0 ) :

        wx.ListCtrl.__init__( self, parent, ID, pos, size, style )
        listmix.ListCtrlAutoWidthMixin.__init__( self )
        listmix.CheckListCtrlMixin.__init__(self)
        self.Bind(wx.EVT_LIST_COL_CLICK, self.OnCheckItem)
        self.Bind(wx.EVT_LIST_COL_CLICK, self.GetSortedOrder)
        self.ordered_variables = data.variables
        self.count = 0
        self.checked_list = []
        self.axis = axis
        self.active = []
        self.active_count = 0

    def OnCheckItem(self, index, flag):
        self.update_ordered_variables()
        self.GetCheckedList()
        self.update_active()
        #dump_info()
        frame.plot.update_plot(log_options=frame.log_options, axis_limits=frame.axis_limits)
    
    def GetCheckedList(self):
        self.checked_list = [index for index in range(self.ItemCount)
                if self.IsChecked(index)]

    def GetSortedOrder(self, event):
        pass

    def set_count(self):
        self.count = self.GetItemCount()

    def update_ordered_variables(self):
        self.ordered_variables = [self.GetItem(itemId=row, col=0).GetText() for row in xrange(self.count)]

    def update_active(self):
        self.active = []
        for index in self.checked_list:
            self.active.append(self.ordered_variables[index])
        self.active.sort()
        self.active_count = len(self.active)


class AxisPanel(wx.Panel, listmix.ColumnSorterMixin):

    def __init__(self, parent, data_object, axis):

        wx.Panel.__init__( self, parent=parent, id=wx.ID_ANY )
        self.axis = axis
        self.createAndLayout(data_object)
        self.list.set_count()

    def createAndLayout(self, data_object):
        sizer = wx.BoxSizer( wx.VERTICAL )
        self.list = SortableListCtrl( self, wx.ID_ANY, axis=self.axis, style=wx.LC_REPORT
                             | wx.BORDER_NONE
                             | wx.LC_EDIT_LABELS
                             | wx.LC_SORT_ASCENDING, )

        sizer.Add(self.list, 1, wx.EXPAND)
        self.populateList(data_object)

        self.itemDataMap = data.variables_stripped # when sorting the list, use the stripped variables

        listmix.ColumnSorterMixin.__init__(self, numColumns=1)
        self.SetSizer(sizer)
        self.SetAutoLayout( True )

    def populateList(self, data_object):
        self.list.InsertColumn(0, axis[self.axis], width=150)
        items = data_object.variables.items()

        for key, data in items:
            index = self.list.InsertStringItem(sys.maxint, data)           
            self.list.SetItemData(index, key)
        self.list.SetColumnWidth( 0, wx.LIST_AUTOSIZE )
    
    def GetListCtrl(self) :
        return self.list

class CanvasPanel(wx.Panel):
    def __init__(self, parent):
        wx.Panel.__init__(self, parent)
        self.figure = Figure(facecolor='white')
        self.axis = self.figure.add_subplot(111)
        self.axis2 = self.axis.twinx()
        self.axis.ticklabel_format(useOffset=False)
        self.axis2.ticklabel_format(useOffset=False)

        self.axis2.axis('off')
        self.canvas = FigureCanvas(self, -1, self.figure)
                
        self.sizer = wx.BoxSizer()
        self.sizer.Add(self.canvas, 1, wx.EXPAND|wx.ALL, 1)
        self.SetSizerAndFit(self.sizer)

    def update_plot(self,log_options, axis_limits):
        self.axis.clear()
        self.axis2.clear()
        self.legendEntries1=[]
        self.legendEntries2=[]
        self.legendText1=[]
        self.legendText2=[]

        if (frame.x1.list.active_count == 1):
            if (log_options[0]):
                self.axis.set_xscale('log')
            else:
                self.axis.ticklabel_format(useOffset=False)

            point_interval = frame.point_interval_widget.GetValue()

            x1_var = frame.x1.list.active[0]
            if (frame.y1.list.active_count > 0):
                self.axis.set_color_cycle(list1)
                if (log_options[1]):
                    self.axis.set_yscale('log')
                for y1_var in frame.y1.list.active:
                    current1, = self.axis.plot(
                            data.df[x1_var].values,
                            data.df[y1_var].values,
                            marker='o',
                            markevery=point_interval,
                            )
                    self.legendEntries1.append(current1)
                    self.legendText1.append(y1_var.replace("'",''))
                    self.axis.set_xlim(data.df[x1_var].min(),data.df[x1_var].max())                    
                    lgd = self.axis.legend(
                            self.legendEntries1, 
                            self.legendText1, 
                            numpoints=1, 
                            loc='upper left',
                            prop={'size':legend_font_size}
                            )
            else:
                self.axis.clear() # remove data

            x1_var = frame.x1.list.active[0]
            self.axis.set_xlabel(x1_var.replace("'",''))
            
            if (frame.y2.list.active_count > 0):
                self.axis.set_color_cycle(list2)
                if (log_options[2]):
                    self.axis2.set_yscale('log')
                else:
                    self.axis2.ticklabel_format(useOffset=False)
                for y2_var in frame.y2.list.active:
                    current2, = self.axis2.plot(
                            data.df[x1_var].values,
                            data.df[y2_var].values,
                            marker='s',
                            markevery=point_interval,
                            )
                    self.axis2.set_xlim(data.df[x1_var].min(),data.df[x1_var].max())                    
                    self.legendEntries2.append(current2)
                    self.legendText2.append(y2_var.replace("'",''))
                    lgd2 = self.axis2.legend(
                            self.legendEntries2, 
                            self.legendText2, 
                            numpoints=1, 
                            loc='upper right',
                            prop={'size':legend_font_size}
                            )
            else:
                self.axis2.clear()
                self.axis2.axis('off')
                
        else:
            self.axis.clear()
            self.axis2.clear() # remove all tics/labels
        

# these are the current frame limits in the canvas
        x1_lims = self.axis.get_xlim()
        y1_lims = self.axis.get_ylim()
        y2_lims = self.axis2.get_ylim()

# these are possible requests to set the limits coming from the "Set axis ranges" window
        x1_min_input = axis_limits[0][0]
        x1_max_input = axis_limits[0][1]
        y1_min_input = axis_limits[1][0]
        y1_max_input = axis_limits[1][1]
        y2_min_input = axis_limits[2][0]
        y2_max_input = axis_limits[2][1]


        if (x1_min_input):
            self.axis.set_xlim(xmin=x1_min_input)
        else:
            x1_min = "{0:.3g}".format(x1_lims[0])
            frame.x1_min.SetValue(x1_min)

        if (x1_max_input):
            self.axis.set_xlim(xmax=x1_max_input)
        else:
            x1_max = "{0:.3g}".format(x1_lims[1])
            frame.x1_max.SetValue(x1_max)

        if (y1_min_input):
            self.axis.set_ylim(ymin=y1_min_input)
        else:
            y1_min = "{0:.3g}".format(y1_lims[0])
            frame.y1_min.SetValue(y1_min)

        if (y1_max_input):
            self.axis.set_ylim(ymax=y1_max_input)
        else:
            y1_max = "{0:.3g}".format(y1_lims[1])
            frame.y1_max.SetValue(y1_max)

        if (y2_min_input):
            self.axis2.set_ylim(ymin=y2_min_input)
        else:
            y2_min = "{0:.3g}".format(y2_lims[0])
            frame.y2_min.SetValue(y2_min)

        if (y2_max_input):
            self.axis2.set_ylim(ymax=y2_max_input)
        else:
            y2_max = "{0:.3g}".format(y2_lims[1])
            frame.y2_max.SetValue(y2_max)



        self.canvas.draw()

class FrameGenerator(wx.Frame):
 
    def __init__(self, parent, id, title, data_object):
        wx.Frame.__init__(self, parent, id, title, size=(-1,-1), pos=(-1,-1))

        menubar = wx.MenuBar()
        fileMenu = wx.Menu()
        fitem = fileMenu.Append(wx.ID_EXIT, 'Quit', 'Quit application')
        self.SetMenuBar(menubar)
        self.Bind(wx.EVT_MENU, self.OnQuit, fitem)

        # put frame on second screen (if it exists)
        monitors = (wx.Display(i) for i in range(wx.Display.GetCount()))
        monitor_sizes = [monitor.GetGeometry().GetSize() for monitor in monitors]
        if (len(monitor_sizes) > 1):
            primary_monitor_width = monitor_sizes[0][0]
            frame_x_location = primary_monitor_width + 50
            frame_y_location = 100
            self.SetPosition((frame_x_location, frame_y_location))

        self.data_object = data_object
       
        container_panel_left = wx.Panel(self, -1)        
        self.x1 = AxisPanel(container_panel_left, self.data_object, axis=0)
        self.y1 = AxisPanel(container_panel_left, self.data_object, axis=1)
        self.y2 = AxisPanel(container_panel_left, self.data_object, axis=2)
        
        vbox_left = wx.BoxSizer(wx.VERTICAL)
        vbox_left.Add(self.x1, proportion=1, flag=wx.EXPAND, border=1)
        vbox_left.Add(self.y1, proportion=2, flag=wx.EXPAND, border=1)
        vbox_left.Add(self.y2, proportion=2, flag=wx.EXPAND, border=1)

        container_panel_left.SetSizer(vbox_left)

        container_panel_options = wx.Panel(self, -1)

# log scale settings
        set_log = wx.StaticBox(container_panel_options, label='Set log scale', pos=(5, 5), size=(120, 100))
        set_log_sizer = wx.StaticBoxSizer(set_log, wx.VERTICAL)

        self.x1_log = wx.CheckBox(container_panel_options, label='x1', pos=(15, 20))
        self.y1_log = wx.CheckBox(container_panel_options, label='y1', pos=(15, 40))
        self.y2_log = wx.CheckBox(container_panel_options, label='y2', pos=(15, 60))

        set_log_sizer.Add(self.x1_log, proportion=0, flag=wx.ALL, border=1)
        set_log_sizer.Add(self.y1_log, proportion=0, flag=wx.ALL, border=1)
        set_log_sizer.Add(self.y2_log, proportion=0, flag=wx.ALL, border=1)

        self.x1_log.Bind(wx.EVT_CHECKBOX, self.SetLogx1)
        self.y1_log.Bind(wx.EVT_CHECKBOX, self.SetLogy1)
        self.y2_log.Bind(wx.EVT_CHECKBOX, self.SetLogy2)
        

# point interval settings
        set_point_interval = wx.StaticBox(container_panel_options, label='Set point interval', pos=(5, 200), size=(120, 80))
        set_point_interval_sizer = wx.StaticBoxSizer(set_point_interval, wx.VERTICAL)

        self.point_interval_widget = wx.SpinCtrl(container_panel_options, value='1', pos=(15, 20), size=(60, -1), min=1, max=40)

        set_point_interval_sizer.Add(self.point_interval_widget, proportion=0, flag=wx.ALL, border=1)

        self.Bind( wx.EVT_SPINCTRL, self.OnSpin )

# scale settings
        set_ranges = wx.StaticBox(container_panel_options, label='Set axis ranges', pos=(5, 300), size=(200, 200))
        set_ranges_sizer = wx.StaticBoxSizer(set_ranges, wx.VERTICAL)

        label_width=60
        input_width=75

        self.x1_max = wx.TextCtrl(container_panel_options,-1,style=wx.TE_PROCESS_ENTER, value='0', size=(input_width, -1))
        self.x1_min = wx.TextCtrl(container_panel_options,-1,style=wx.TE_PROCESS_ENTER, value='0', size=(input_width, -1))
        self.y1_max = wx.TextCtrl(container_panel_options,-1,style=wx.TE_PROCESS_ENTER, value='0', size=(input_width, -1))
        self.y1_min = wx.TextCtrl(container_panel_options,-1,style=wx.TE_PROCESS_ENTER, value='0', size=(input_width, -1))
        self.y2_max = wx.TextCtrl(container_panel_options,-1,style=wx.TE_PROCESS_ENTER, value='0', size=(input_width, -1))
        self.y2_min = wx.TextCtrl(container_panel_options,-1,style=wx.TE_PROCESS_ENTER, value='0', size=(input_width, -1))

        flex_x1 = wx.FlexGridSizer(rows=2, cols=2, vgap=1, hgap=1)
        x1_min_label = wx.StaticText(container_panel_options, label="x1 min", size=(label_width, -1))
        x1_max_label = wx.StaticText(container_panel_options, label="x1 max", size=(label_width, -1))
        flex_x1.AddMany([(x1_max_label), (self.x1_max, 1,), (x1_min_label), (self.x1_min, 1,)])
        set_ranges_sizer.Add(flex_x1, proportion=0, flag=wx.ALL, border=1)

        flex_y1 = wx.FlexGridSizer(rows=2, cols=2, vgap=1, hgap=1)
        y1_min_label = wx.StaticText(container_panel_options, label="y1 min", size=(label_width, -1))
        y1_max_label = wx.StaticText(container_panel_options, label="y1 max", size=(label_width, -1))
        flex_y1.AddMany([(y1_max_label), (self.y1_max, 1,), (y1_min_label), (self.y1_min, 1,)])
        set_ranges_sizer.Add(flex_y1, proportion=0, flag=wx.ALL, border=1)        

        flex_y2 = wx.FlexGridSizer(rows=2, cols=2, vgap=1, hgap=1)
        y2_min_label = wx.StaticText(container_panel_options, label="y2 min", size=(label_width, -1))
        y2_max_label = wx.StaticText(container_panel_options, label="y2 max", size=(label_width, -1))
        flex_y2.AddMany([(y2_max_label), (self.y2_max, 1,), (y2_min_label), (self.y2_min, 1,)])
        set_ranges_sizer.Add(flex_y2, proportion=0, flag=wx.ALL, border=1)

        reset_ranges = wx.Button(container_panel_options, -1, 'Reset', size=(140, -1))
        set_ranges_sizer.Add(reset_ranges, proportion=0, flag=wx.ALL, border=10)
        reset_ranges.Bind(wx.EVT_BUTTON, self.OnResetRanges)


        self.x1_min.Bind(wx.EVT_TEXT_ENTER, self.Onx1Min)
        self.x1_min.Bind(wx.EVT_KILL_FOCUS, self.Onx1Min) 
        self.x1_max.Bind(wx.EVT_TEXT_ENTER, self.Onx1Max)
        self.x1_max.Bind(wx.EVT_KILL_FOCUS, self.Onx1Max)
        self.y1_min.Bind(wx.EVT_TEXT_ENTER, self.Ony1Min)
        self.y1_min.Bind(wx.EVT_KILL_FOCUS, self.Ony1Min) 
        self.y1_max.Bind(wx.EVT_TEXT_ENTER, self.Ony1Max)
        self.y1_max.Bind(wx.EVT_KILL_FOCUS, self.Ony1Max)
        self.y2_min.Bind(wx.EVT_TEXT_ENTER, self.Ony2Min)
        self.y2_min.Bind(wx.EVT_KILL_FOCUS, self.Ony2Min) 
        self.y2_max.Bind(wx.EVT_TEXT_ENTER, self.Ony2Max)
        self.y2_max.Bind(wx.EVT_KILL_FOCUS, self.Ony2Max)

# plot_step.pl export settings
        plot_step_export = wx.StaticBox(container_panel_options, label='plot_step.pl export', pos=(5, 200), size=(-1, -1))
        plot_step_export_sizer = wx.StaticBoxSizer(plot_step_export, wx.VERTICAL)

        self.rb_x11 = wx.RadioButton(container_panel_options, label='x11', style=wx.RB_GROUP)
        self.rb_png = wx.RadioButton(container_panel_options, label='png')
        self.rb_pdf = wx.RadioButton(container_panel_options, label='pdf')

        self.rb_x11.Bind(wx.EVT_RADIOBUTTON, self.SetVal)
        self.rb_png.Bind(wx.EVT_RADIOBUTTON, self.SetVal)
        self.rb_pdf.Bind(wx.EVT_RADIOBUTTON, self.SetVal)
        
        display_string = wx.Button(container_panel_options, -1, 'Display command', size=(140, -1))       
        run_command = wx.Button(container_panel_options, -1, 'Run plot_step.pl', size=(140, -1))       

        display_string.Bind(wx.EVT_BUTTON, self.OnDisplayString)
        run_command.Bind(wx.EVT_BUTTON, self.OnRunCommand)
        
        plot_step_export_sizer.Add(self.rb_x11, proportion=0, flag=wx.ALL, border=1)
        plot_step_export_sizer.Add(self.rb_png, proportion=0, flag=wx.ALL, border=1)
        plot_step_export_sizer.Add(self.rb_pdf, proportion=0, flag=wx.ALL, border=1)
        plot_step_export_sizer.Add(display_string, proportion=0, flag=wx.ALL, border=1)
        plot_step_export_sizer.Add(run_command, proportion=0, flag=wx.ALL, border=1)

# refresh plot feature
        refresh = wx.StaticBox(container_panel_options, label='Continually refresh data', pos=(5, 200), size=(-1, -1))
        refresh_sizer = wx.StaticBoxSizer(refresh, wx.VERTICAL)
        self.start_refresh_button = start_refresh_button = wx.Button(container_panel_options, -1, 'Start refresh', size=(140, -1))       
        self.stop_refresh_button = stop_refresh_button = wx.Button(container_panel_options, -1, 'Stop refresh', size=(140, -1))       
        self.stop_refresh_button.Disable

        self.refresh_plot_step_output = False # keep a separate variable for this (to avoid the information being destroyed along with the thread)

        refresh_sizer.Add(start_refresh_button, proportion=0, flag=wx.ALL, border=1)
        refresh_sizer.Add(stop_refresh_button, proportion=0, flag=wx.ALL, border=1)

        start_refresh_button.Bind(wx.EVT_BUTTON, self.start_refresh)
        stop_refresh_button.Bind(wx.EVT_BUTTON, self.stop_refresh)

        vbox_options = wx.BoxSizer(wx.VERTICAL)
        vbox_options.Add(set_log_sizer, proportion=0, flag=wx.TOP, border=1)
        vbox_options.Add(set_point_interval_sizer, proportion=0, flag=wx.TOP, border=1)
        vbox_options.Add(set_ranges_sizer, proportion=0, flag=wx.TOP, border=1)
        vbox_options.Add(plot_step_export_sizer, proportion=0, flag=0, border=1)
        vbox_options.Add(refresh_sizer, proportion=0, flag=0, border=1)
        container_panel_options.SetSizer(vbox_options)

        container_panel_right = wx.Panel(self, -1)
        self.plot = CanvasPanel(container_panel_right)                                      

        vbox_right = wx.BoxSizer(wx.VERTICAL)
        vbox_right.Add(self.plot, proportion=1, flag=wx.EXPAND, border=1)
        container_panel_right.SetSizer(vbox_right)

        hbox = wx.BoxSizer(wx.HORIZONTAL)
        hbox.Add(container_panel_left, proportion=1, flag=wx.EXPAND, border=1)        
        hbox.Add(container_panel_options, proportion=0, flag=wx.EXPAND, border=1)        
        hbox.Add(container_panel_right, proportion=3, flag=wx.EXPAND, border=1)
        self.SetSizerAndFit(hbox)
        self.Show()
        #self.Maximize(True)

        self.log_options = [0,0,0] # x1, y1, y2: 0-->linear scale, 1-->log scale
        self.axis_limits = [[None, None], [None, None], [None, None]] # x1, y1, y2: 0-->linear scale, 1-->log scale
        self.axis_limits_reset = [[None, None], [None, None], [None, None]]

        # subscribe to "update"
        Publisher().subscribe(self.updateDisplay, "update")

    def SetLogx1(self, event):
        sender = event.GetEventObject()
        isChecked = sender.GetValue()
        
        if isChecked:
            self.log_options[0] = 1
        else: 
            self.log_options[0] = 0
        self.call_plot_upate()

    def SetLogy1(self, event):
        sender = event.GetEventObject()
        isChecked = sender.GetValue()
        
        if isChecked:
            self.log_options[1] = 1
        else:
            self.log_options[1] = 0
        self.call_plot_upate()

    def SetLogy2(self, event):
        sender = event.GetEventObject()
        isChecked = sender.GetValue()
        
        if isChecked:
            self.log_options[2] = 1
        else:
            self.log_options[2] = 0
        self.call_plot_upate()

    def OnSpin(self, event):
        self.call_plot_upate()

    def Onx1Min(self, event):
        value = float(self.x1_min.GetValue())
        self.axis_limits[0][0] = value
        self.call_plot_upate()

    def Onx1Max(self, event):
        value = float(self.x1_max.GetValue())
        self.axis_limits[0][1] = value
        self.call_plot_upate()

    def Ony1Min(self, event):
        value = float(self.y1_min.GetValue())
        self.axis_limits[1][0] = value
        self.call_plot_upate()

    def Ony1Max(self, event):
        value = float(self.y1_max.GetValue())
        self.axis_limits[1][1] = value
        self.call_plot_upate()

    def Ony2Min(self, event):
        value = float(self.y2_min.GetValue())
        self.axis_limits[2][0] = value
        self.call_plot_upate()

    def Ony2Max(self, event):
        value = float(self.y2_max.GetValue())
        self.axis_limits[2][1] = value
        self.call_plot_upate()

    def OnResetRanges(self, event):
        # clear the record of user input
        self.axis_limits = [[None, None], [None, None], [None, None]]
        self.call_plot_upate()

    def SetVal(self, event):    
        use_x11 = self.rb_x11.GetValue()
        use_png = self.rb_png.GetValue()
        use_pdf = self.rb_pdf.GetValue()

    def OnDisplayString(self, event):
        plot_step_string = self.gen_plot_step_string()
        wx.MessageBox(plot_step_string, 'String for plot_step.pl',
                wx.OK)

    def OnRunCommand(self, event):
        if not (len(frame.y1.list.active) == 0 and len(frame.y2.list.active) == 0):
            plot_step_string = self.gen_plot_step_string()
            subprocess.call(plot_step_string, shell=True)

    def start_refresh(self, event):
        self.thread = RefreshThread()
        btn = event.GetEventObject()
        btn.Disable()
        frame.stop_refresh_button.Enable()
        self.refresh_plot_step_output = True

    def stop_refresh(self, event):
        frame.thread.refresh_state = False
        btn = event.GetEventObject()
        btn.Disable()
        frame.start_refresh_button.Enable()
        self.refresh_plot_step_output = False

    def updateDisplay(self, msg):
        t = msg.data
        if isinstance(t, int):
            # if we receive an integer message then reload
            # re-read the data
            try:
                self.data_object.step_file = open(self.data_object.step_file_path)
                self.data_object.df=pd.read_csv(self.data_object.step_file, comment='#')[1:]
                # convert all but header labels to float values
                self.data_object.df = self.data_object.df[1:].astype(float)
                self.data_object.step_file.close()

                self.data_object.df=pd.read_csv('output/output_step.csv', comment='#')[1:]
                self.data_object.df = self.data_object.df[1:].astype(float)
                self.call_plot_upate()

            except:
                pass
                #sys.exit('ERROR: output/output_step.csv does not exist') 

        else:
            # if we receive a non-integer message then refresh thread will have stopped
            self.call_plot_upate()
            self.start_refresh_button.Enable()
 
    def call_plot_upate(self):
        frame.plot.update_plot(log_options=self.log_options, axis_limits=self.axis_limits)

    def gen_plot_step_string(self):

        x1_list = ','.join(frame.x1.list.active)
        y1_list = ','.join(frame.y1.list.active)
        y2_list = ','.join(frame.y2.list.active)

        x1_min = frame.x1_min.GetValue()
        x1_max = frame.x1_max.GetValue()

        y1_min = frame.y1_min.GetValue()
        y1_max = frame.y1_max.GetValue()

        y2_min = frame.y2_min.GetValue()
        y2_max = frame.y2_max.GetValue()

        x1_range = '[{}:{}]'.format(x1_min, x1_max) if (frame.x1.list.active) else ''
        y1_range = '[{}:{}]'.format(y1_min, y1_max) if (frame.y1.list.active) else ''
        y2_range = '[{}:{}]'.format(y2_min, y2_max) if (frame.y2.list.active) else ''
        
        if len(frame.y2.list.active)==0:
            combined_list = ':'.join([x1_list+x1_range, y1_list+y1_range])
        else:
            combined_list = ':'.join([x1_list+x1_range, y1_list+y1_range, y2_list+y2_range])

        plot_step_options = []

        #if (self.refresh_plot_step_output):
        #    plot_step_options.append("--refresh")
        if (frame.x1_log.GetValue()):
            plot_step_options.append("--gnuplotoption 'set log x'")
        if (frame.y1_log.GetValue()):
            plot_step_options.append("--gnuplotoption 'set log y'")
        if (frame.y2_log.GetValue()):
            plot_step_options.append("--gnuplotoption 'set log y2'")

        if (frame.rb_png.GetValue()):
            plot_step_options.append("-o chart.png")
        if (frame.rb_pdf.GetValue()):
            plot_step_options.append("-o chart.pdf")

        point_interval=frame.point_interval_widget.GetValue()
        if (frame.point_interval_widget.GetValue() > 1):
            plot_step_options.append("--linespoints --pointinterval "+str(point_interval))

        combined_options = ' '.join(plot_step_options)

        command = ' '.join(['./plot_step',combined_options,combined_list])
        
        return command
    
    def OnQuit(self, event):
        self.Close()

if __name__ == "__main__":

    data = Data()
    app = wx.App(False)
    
    directory_name = os.path.basename(os.getcwd())
    frame = FrameGenerator(None, -1, title=directory_name, data_object=data)

    # This will select '<t>' as default x1 axis variable (if it exists)
    step = 0
    if (step == 0):
        try:
            frame.x1.list.CheckItem(data.inverted["'<timestep>'"]-1, True)
        except:
            pass
        step+=1

    frame.Show()

    app.MainLoop()
