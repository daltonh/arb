#!/usr/bin/env python

# user interface for visualising contents of output/output_step.csv
# requires additional python libraries which can be installed following these steps

# ubuntu:
# run the script "misc/track/install_plot_dependencies_on_ubuntu.sh"

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
import re

from cStringIO import StringIO

from threading import Thread
from wx.lib.pubsub import Publisher

import pandas as pd
import numpy as np

from distutils.version import LooseVersion
import matplotlib
matplotlib.use('WXAgg')
from matplotlib.backends.backend_wxagg import FigureCanvasWxAgg as FigureCanvas
from matplotlib.figure import Figure

# to output wx version
#print wx.__version__

operating_system = os.uname()[0]

show_plot_step_export = 0
grid_style = 0
rasterized_option = True # whether to rasterize pdf data (not the axes and labels, just the data), good when there is a lot of data to show

#if (LooseVersion(matplotlib.__version__) >= LooseVersion("1.4.2")):
#    import matplotlib.style
#    matplotlib.style.use('ggplot')
#    grid_style = 1

old_wx_version = False
running_wx_version = LooseVersion(wx.__version__)
old_wx_version_threshold = "2.9.0"
if running_wx_version < old_wx_version_threshold:
    old_wx_version = True

# directory storing simulation data
# see misc/track/track_main.pl
run_archive = '.archive'

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
'#58FAAC',
'#FAAC58',
'#848484',
'#82FA58',
'#D358F7',
'#FA5858',
'#FA5882',
'#5858FA',
]

# marker transparency
y1_alpha = 0.7
y2_alpha = 0.7

# font size for legend
legend_font_size=11

# string for storing commit id of present dataset
global_commit = ''

# need regex to deal with [l=1,r=1] combinations
# this is a negative look ahead for ,r and ,l
sep_string=",(?!r|l)"

class Data():
    def __init__(self, step_file_path):

        # read output_step.csv into pandas data frame
        # ignore the row of dimension strings ([1:])
        # (in future, could use dimension strings to automatically show dimensions in plot labels)
        #self.step_file_path = 'output/output_step.csv'
        self.step_file_path = step_file_path
        self.show_markers = True
        self.show_newtsteps = False
        self.open_data()
        self.process_data()

    def open_data(self):
        global load_blank

        if (load_blank):
            self.step_file = blank_csv
        elif (os.path.isfile(self.step_file_path)):
            self.step_file = open(self.step_file_path)
        else:
            print "ERROR: {} does not exist".format(self.step_file_path)
            print "Try running a with an archived simulation: `track plot <commit-id>`"
            print "or equivalently run `./misc/track/plot.py <commit-id>`"
            
            available = next(os.walk(run_archive))[1]
            if (available):
                print "Available archived simulations are"

            for entry in available:
                if re.match(r'stash_storage', entry): #re.match matches from start of string
                    pass
                else:
                    print "\t{}".format(entry)

            sys.exit() 
        
    def process_data(self):
        global load_blank
        if (load_blank):
            self.df=pd.read_csv(blank_csv, comment='#')[1:]
        else:
            #self.df=pd.read_csv(self.step_file, comment='#', low_memory=False)[1:]
            self.df=pd.read_csv(self.step_file, comment='#', engine='python', sep=sep_string)[1:]
        load_blank = 0
        # convert all but header labels to float values
        self.df = self.df.astype(float)
        if (not self.show_newtsteps and not load_blank):
            self.df = self.df.groupby("'<timestep>'", as_index=False).nth(-1)
        self.step_file.close()
   
        # dictionaries needed for ColumnSorterMixin
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

    def change_dataset(self, path):
        self.step_file_path = path
        self.open_data()
        self.process_data()
    
    def example_method(self):
        print 'was called'

class RefreshThread(Thread):
    def __init__(self):
        self.refresh_state = True
        Thread.__init__(self)
        self.daemon = True # thread will die on window close
        self.start()    # start the thread
 
    def run(self):
        # This is the code executing in the new thread.
        while (self.refresh_state):
            time.sleep(5) # wait n seconds
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

    def clear(self):
        #self.ClearAll()
        self.DeleteAllItems()


class AxisPanel(wx.Panel, listmix.ColumnSorterMixin):

    def __init__(self, parent, data_object, axis):

        wx.Panel.__init__( self, parent=parent, id=wx.ID_ANY )
        self.axis = axis
        self.create(data_object)
        self.layout(data_object)
        self.run_set_count()
        
    def create(self, data_object):
        self.sizer = wx.BoxSizer( wx.VERTICAL )
        self.list = SortableListCtrl( self, wx.ID_ANY, axis=self.axis, style=wx.LC_REPORT
                             | wx.BORDER_NONE
                             | wx.LC_EDIT_LABELS
                             | wx.LC_SORT_ASCENDING, )

        self.sizer.Add(self.list, 1, wx.EXPAND)
        self.list.InsertColumn(0, axis[self.axis], width=150)


    def layout(self, data_object):
        self.populateList(data_object)
        self.itemDataMap = data.variables_stripped # when sorting the list, use the stripped variables
        listmix.ColumnSorterMixin.__init__(self, numColumns=1)
        self.SetSizer(self.sizer)
        self.SetAutoLayout( True )

    def run_set_count(self):
        self.list.set_count()
       
    def populateList(self, data_object):
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
        self.axis.set_title(global_commit,fontdict={'fontsize': 8})
        
        self.axis2 = self.axis.twinx()
        self.axis.ticklabel_format(useOffset=False)
        self.axis2.ticklabel_format(useOffset=False)

        self.axis2.axis('off')
        self.canvas = FigureCanvas(self, -1, self.figure)
                
        self.sizer = wx.BoxSizer()
        self.sizer.Add(self.canvas, 1, wx.EXPAND|wx.ALL, 1)
        self.SetSizerAndFit(self.sizer)

    def update_plot(self,log_options, axis_limits):
        global global_commit

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

            marker_y1 = None
            marker_y2 = None
            if (data.show_markers):
                marker_y1 = 'o'
                marker_y2 = '^'

            x1_var = frame.x1.list.active[0]
            if (frame.y1.list.active_count > 0):
                self.axis.set_color_cycle(list1)
                if (log_options[1]):
                    self.axis.set_yscale('log')
                for y1_var in frame.y1.list.active:
                    current1, = self.axis.plot(
                            data.df[x1_var].values,
                            data.df[y1_var].values,
                            marker=marker_y1,
                            markevery=point_interval,
                            alpha = y1_alpha,
                            rasterized=rasterized_option
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
                self.axis2.set_color_cycle(list2)
                if (log_options[2]):
                    self.axis2.set_yscale('log')
                else:
                    self.axis2.ticklabel_format(useOffset=False)
                for y2_var in frame.y2.list.active:
                    current2, = self.axis2.plot(
                            data.df[x1_var].values,
                            data.df[y2_var].values,
                            marker=marker_y2,
                            markevery=point_interval,
                            alpha = y2_alpha,
                            rasterized=rasterized_option
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
            pass
            self.axis.clear()
            #self.axis2.clear() # remove all tics/labels
        

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

        self.axis.set_title(global_commit,fontdict={'fontsize': 8})
        self.axis2.patch.set_visible(False)


        if ((frame.y1.list.active_count > 0) and (frame.y2.list.active_count > 0)):
            if (grid_style):
                self.axis.grid(b=False)
                self.axis2.grid(b=False)
        elif (frame.y1.list.active_count > 0):
            if (grid_style):
                self.axis.grid(b=True)
                self.axis2.grid(b=False)
            self.axis2.get_yaxis().set_ticks([])
        elif (frame.y2.list.active_count > 0):
            if (grid_style):
                self.axis.grid(b=False)
                self.axis2.grid(b=True)
            self.axis.get_yaxis().set_ticks([])
        else:
            if (grid_style):
                self.axis.grid(b=False)
                self.axis2.grid(b=False)
            self.axis.get_yaxis().set_ticks([])
            self.axis2.get_yaxis().set_ticks([])

        self.canvas.draw()

class FrameGenerator(wx.Frame):
 
    def __init__(self, parent, id, title, data_object):
        wx.Frame.__init__(self, parent, id, title, size=(-1,-1), pos=(-1,-1))

        menubar = wx.MenuBar()
        fileMenu = wx.Menu()
        fitem = fileMenu.Append(wx.ID_EXIT, 'Quit', 'Quit application')
        self.SetMenuBar(menubar)
        self.Bind(wx.EVT_MENU, self.OnQuit, fitem)
        self.block = 0 # use this to block any incoming subscribed-to messages

        # put frame on second screen (if it exists)
        monitors = (wx.Display(i) for i in range(wx.Display.GetCount()))
        self.monitor_sizes = [monitor.GetGeometry().GetSize() for monitor in monitors]
        if (len(self.monitor_sizes) > 1):
            primary_monitor_width = self.monitor_sizes[0][0]
            self.frame_x_location = primary_monitor_width + 50
            self.frame_y_location = -50
            self.SetPosition((self.frame_x_location, self.frame_y_location))

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


# change dataset feature
        change_dataset_box = wx.StaticBox(container_panel_options, label='Archive integration', pos=(5, 200), size=(-1, -1))
        change_dataset_sizer = wx.StaticBoxSizer(change_dataset_box, wx.VERTICAL)
        self.change_dataset_button = change_dataset_button = wx.Button(container_panel_options, -1, 'Change dataset', size=(140, -1))       
        
        change_dataset_button.Bind(wx.EVT_BUTTON, self.start_refresh)
        change_dataset_sizer.Add(change_dataset_button, proportion=0, flag=wx.ALL, border=1)

        change_dataset_button.Bind(wx.EVT_BUTTON, self.change_dataset)

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
        set_point_interval = wx.StaticBox(container_panel_options, label='Set marker interval', pos=(5, 200), size=(120, 80))
        set_point_interval_sizer = wx.StaticBoxSizer(set_point_interval, wx.VERTICAL)

        self.point_interval_widget = wx.SpinCtrl(container_panel_options, value='1', pos=(15, 20), size=(80, -1), min=1, max=1e6, style=wx.TE_PROCESS_ENTER)

        # enable TAB and ENTER on mac (works already on ubuntu)
        # see https://groups.google.com/forum/#!topic/wxpython-users/Gud8PI6n-4E

        # if the version of wx is "old" then bypass this section
        if (operating_system == 'Darwin' and not old_wx_version):
            # the following line likely works with all wxpython v2.9.* but not with all v2.8.*
            self.tmp_txtctrl = self.point_interval_widget.GetChildren()[0]
            
            # the following alternative lines likely work with all wxpython v2.9.* and v2.8.* (but not tested yet with v2.8.*)
            #self.tmp_children_list = list(self.point_interval_widget.GetChildren())
            #self.tmp_txtctrl = self.tmp_children_list[0]
            
            self.tmp_txtctrl.SetWindowStyle(self.tmp_txtctrl.GetWindowStyle() | wx.TE_PROCESS_ENTER)

        self.show_markers_box = wx.CheckBox(container_panel_options, label='Show markers', pos=(15, 20))
        self.show_markers_box.SetValue(True)
        self.show_newtsteps_box = wx.CheckBox(container_panel_options, label='Show newtsteps', pos=(15, 20))

        set_point_interval_sizer.Add(self.point_interval_widget, proportion=0, flag=wx.ALL, border=1)
        set_point_interval_sizer.Add(self.show_markers_box, proportion=0, flag=wx.ALL, border=1)
        set_point_interval_sizer.Add(self.show_newtsteps_box, proportion=0, flag=wx.ALL, border=1)

        self.Bind( wx.EVT_SPINCTRL, self.OnSpin )
        if (operating_system == 'Darwin' and not old_wx_version):
            self.tmp_txtctrl.Bind(wx.EVT_TEXT_ENTER, self.OnSpin)
    
        self.show_markers_box.Bind(wx.EVT_CHECKBOX, self.ShowMarkers)
        self.show_newtsteps_box.Bind(wx.EVT_CHECKBOX, self.ShowNewtsteps)

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

# export settings

        matplotlib_export = wx.StaticBox(container_panel_options, label='matplotlib export', pos=(5, 200), size=(-1, -1))
        matplotlib_export_sizer = wx.StaticBoxSizer(matplotlib_export, wx.VERTICAL)
        self.matplotlib_rb_png = wx.RadioButton(container_panel_options, label='png')
        self.matplotlib_rb_png.SetValue(1)
        self.matplotlib_rb_pdf = wx.RadioButton(container_panel_options, label='pdf')
        self.export_filename = wx.TextCtrl(container_panel_options,-1,style=wx.TE_PROCESS_ENTER, value='figure.png', size=(140, -1))

        export_string = wx.Button(container_panel_options, -1, 'Export', size=(140, -1))       
        matplotlib_export_sizer.Add(self.matplotlib_rb_png, proportion=0, flag=wx.ALL, border=1)
        matplotlib_export_sizer.Add(self.matplotlib_rb_pdf, proportion=0, flag=wx.ALL, border=1)
        matplotlib_export_sizer.Add(self.export_filename, proportion=0, flag=wx.ALL, border=1)
        matplotlib_export_sizer.Add(export_string, proportion=0, flag=wx.ALL, border=1)

        self.matplotlib_rb_png.Bind(wx.EVT_RADIOBUTTON, self.SetValMatplotlibExport)
        self.matplotlib_rb_pdf.Bind(wx.EVT_RADIOBUTTON, self.SetValMatplotlibExport)
        export_string.Bind(wx.EVT_BUTTON, self.OnExportString)

# plot_step.pl export settings

        if (show_plot_step_export):
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
    
        if (show_plot_step_export):
            self.refresh_plot_step_output = False # keep a separate variable for this (to avoid the information being destroyed along with the thread)

        refresh_sizer.Add(start_refresh_button, proportion=0, flag=wx.ALL, border=1)
        refresh_sizer.Add(stop_refresh_button, proportion=0, flag=wx.ALL, border=1)

        start_refresh_button.Bind(wx.EVT_BUTTON, self.start_refresh)
        stop_refresh_button.Bind(wx.EVT_BUTTON, self.stop_refresh)


        vbox_options = wx.BoxSizer(wx.VERTICAL)
        vbox_options.Add(change_dataset_sizer, proportion=0, flag=0, border=1)
        vbox_options.Add(set_log_sizer, proportion=0, flag=wx.TOP, border=1)
        vbox_options.Add(set_point_interval_sizer, proportion=0, flag=wx.TOP, border=1)
        vbox_options.Add(set_ranges_sizer, proportion=0, flag=wx.TOP, border=1)
        vbox_options.Add(matplotlib_export_sizer, proportion=0, flag=0, border=1)
        if (show_plot_step_export):
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

        self.selector = 0

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

    def ShowNewtsteps(self, event):
        data.show_newtsteps = self.show_newtsteps_box.GetValue()
        data.open_data()
        data.process_data()
        self.call_plot_upate()

    def ShowMarkers(self, event):
        data.show_markers = self.show_markers_box.GetValue()
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


    def SetValMatplotlibExport(self, event):
        previous_string = self.export_filename.GetValue()
        previous_name, previous_extension = os.path.splitext(previous_string)
        if (self.matplotlib_rb_png.GetValue()):
            new_string = previous_name+'.png'
        elif (self.matplotlib_rb_pdf.GetValue()):
            new_string = previous_name+'.pdf'
        self.export_filename.SetValue(new_string)

    def OnExportString(self, event):
        filename = self.export_filename.GetValue()
        file_exists = os.path.isfile(filename)
        overwrite = True

        if(file_exists):
            message = "{} already exists\n\nOverwrite it?".format(filename)
            dlg = wx.MessageDialog(None,message, 'Info', 
            wx.YES_NO | wx.ICON_QUESTION)
            overwrite = dlg.ShowModal() == wx.ID_YES
            dlg.Destroy()

        if (overwrite or not file_exists):
            if (self.matplotlib_rb_png.GetValue()):
                    frame.plot.figure.savefig(filename, dpi=300)
            if (self.matplotlib_rb_pdf.GetValue()):
                    frame.plot.figure.savefig(filename, dpi=300)

    def OnDisplayString(self, event):
        plot_step_string = self.gen_plot_step_string()
        wx.MessageBox(plot_step_string, 'String for plot_step.pl',
                wx.OK)

    def OnRunCommand(self, event):
        if not (len(frame.y1.list.active) == 0 and len(frame.y2.list.active) == 0):
            plot_step_string = self.gen_plot_step_string()
            subprocess.call(plot_step_string, shell=True)

    def start_refresh(self, event):
        self.block = 0
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

    def force_stop_refresh(self):
        frame.stop_refresh_button.Disable()
        frame.start_refresh_button.Enable()
        if 'frame.thread' in locals():
            frame.thread.refresh_state = False
        self.refresh_plot_step_output = False
        self.block = 1

    def updateDisplay(self, msg):
        global global_commit

        t = msg.data
        if (isinstance(t, int) and not self.block):
            # if we receive an integer message then reload
            # re-read the data
            try:
                self.data_object.step_file = open(self.data_object.step_file_path)
                #self.data_object.df=pd.read_csv(self.data_object.step_file, comment='#',low_memory=False)[1:]
                self.data_object.df=pd.read_csv(self.data_object.step_file, comment='#', engine='python', sep=sep_string)[1:]
                # convert all but header labels to float values
                self.data_object.df = self.data_object.df.astype(float)
                self.data_object.step_file.close()

                if (global_commit):
                    path = os.path.join(run_archive,global_commit,'output','output_step.csv')
                    #self.data_object.df=pd.read_csv(path, comment='#', low_memory=False)[1:]
                    self.data_object.df=pd.read_csv(path, comment='#', engine='python', sep=sep_string)[1:]
                else:
                    #self.data_object.df=pd.read_csv('output/output_step.csv', comment='#', low_memory=False)[1:]
                    self.data_object.df=pd.read_csv('output/output_step.csv', comment='#', engine='python', sep=sep_string)[1:]
                self.data_object.df = self.data_object.df.astype(float)
                self.call_plot_upate()
            except:
                pass
                #sys.exit('ERROR: output/output_step.csv does not exist') 

        else:
            # if we receive a non-integer message then refresh thread will have stopped
            #self.call_plot_upate()
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

    def change_dataset(self, event):
        self.selector = DatasetSelection()
        self.selector.Show()
    
    def OnQuit(self, event):
        frame.selector.Close() 
        frame.selector.Destroy() 
        self.Close()
        self.Destroy()

class DatasetSelection(wx.Frame):
     def __init__(self):
          wx.Frame.__init__(self, None, title="Dataset selection", size=(1350,400))
          if (len(frame.monitor_sizes) > 1):
              self.SetPosition((frame.frame_x_location, frame.frame_y_location))
          container_panel_left = wx.Panel(self, -1)
          self.available = DataSelectPanel(container_panel_left, axis=0)
          vbox_left = wx.BoxSizer(wx.VERTICAL)
          vbox_left.Add(self.available, proportion=1, flag=wx.EXPAND, border=1)
          container_panel_left.SetSizer(vbox_left)

class DataSelectPanel(wx.Panel, listmix.ColumnSorterMixin):

    def __init__(self, parent, axis):

        wx.Panel.__init__( self, parent=parent, id=wx.ID_ANY )
        self.archive_data = {}
        self.archive_data_for_column_sorting = {}
        self.axis = axis
        self.create()
        self.get_archive_data()
        self.populateList()
        #self.itemDataMap = self.archive_data
        self.itemDataMap = self.archive_data_for_column_sorting #enable sorting by unix timestamp
        listmix.ColumnSorterMixin.__init__(self, 7)
        self.Bind(wx.EVT_LIST_COL_CLICK, self.OnColClick, self.list)
        self.SortListItems(col=3, ascending=0) # here we set the default sort by ascending descending time, i.e. column 3
        
        self.SetSizer(self.sizer)
        self.SetAutoLayout( True )

    def create(self):
        self.sizer = wx.BoxSizer( wx.VERTICAL )
        self.list = SingleSelectListCtrl( self, wx.ID_ANY, axis=self.axis, style=wx.LC_REPORT
                             | wx.BORDER_NONE
                             | wx.LC_SINGLE_SEL
                             | wx.LC_SORT_ASCENDING, )

        self.sizer.Add(self.list, 1, wx.EXPAND)

    def get_archive_data(self):
        available = next(os.walk(run_archive))[1]

        # don't show stash storage in the dialogue box
        for entry in available:
            if re.match(r'stash_storage', entry): #re.match matches from start of string
                available.remove(entry)

        # make the data array first
        for (i, simulation) in enumerate(available):
            
            commit_message = os.popen(
                    "git log --format=%B -n 1 {0} | head -1".format(simulation)
                    ).read()
            commit_message = commit_message.replace('\n','')
           
            note = os.popen(
                    "git log --no-walk --oneline --show-notes=track {0} 2>/dev/null | awk '/Notes \(track\)/{{getline; print}}'".format(simulation)
                    ).read()
            note = note.replace('\n','')
            note = note.lstrip()

            relative_time = os.popen(
                    "git show -s --format='%cr' {0}".format(simulation)
                    ).read()
            relative_time = relative_time.replace('\n','')

            unix_timestamp = os.popen(
                    "git show -s --format='%at' {0}".format(simulation)
                    ).read()
            unix_timestamp = unix_timestamp.replace('\n','')

            path_scr = os.path.join(run_archive, simulation, 'output', 'output.scr')
            path_csv = os.path.join(run_archive, simulation, 'output', 'output_step.csv')

            timestep_max = ''
            
            if (os.path.isfile(path_scr) and os.path.isfile(path_csv)):
                tail_scr = os.popen(
                    "tail -10 {0}".format(path_scr)
                    ).read()
                tail_scr.rstrip()

                head_scr = os.popen(
                        "head -3 {0}".format(path_scr)
                        ).read()
                head_scr.rstrip()

                tail_csv = os.popen(
                    "tail -1 {0}".format(path_csv)
                    ).read()
                tail_csv.rstrip()

                head_csv = os.popen(
                    "head -15 {0}".format(path_csv)
                    ).read()
                head_csv.rstrip()

                match = re.search( r'WARNING: the simulation was stopped prematurely by the user using a stop file', tail_scr)
                if (match):
                    status = 'killed'
                else:
                    status = "--"

                match = re.search( r'ERROR: there was an error in one of the solver routines', tail_scr)
                if (match):
                    status = 'convergence error'

                match = re.search( r'SUCCESS: the simulation finished gracefully', tail_scr)
                if (match):
                    status = 'completed'

                match = re.search( r'total wall time = (.*): .*total cpu time = (.*)', tail_scr)
                if (match):
                    wall_time = match.group(1).lstrip().rstrip()
                    cpu_time = match.group(2).lstrip().rstrip()
                    wall_time_output = "{0:.1f}".format(float(wall_time)/60.0/60.0)
                    cpu_time_output = "{0:.1f}".format(float(cpu_time)/60.0/60.0)
                else:
                    wall_time_output = "--"
                    cpu_time_output = "--"
                
                match = re.search( r'(\d+) threads in use', head_scr)
                if (match):
                    threads = match.group(1)
                    thread_count = threads
                else:
                    thread_count = "1"


                timestep_present = re.search(r'<timestep>', head_csv)
                if (timestep_present):
                    timestep_max = tail_csv.split(',')[0]

            else:
                status = ''
                wall_time_output = ''
                cpu_time_output = ''
                thread_count = ''
                timestep_max = ''

            self.archive_data.update({i: (simulation, commit_message, note, relative_time, status, timestep_max, wall_time_output, cpu_time_output, thread_count)})
            self.archive_data_for_column_sorting.update({i: (simulation, commit_message, note, unix_timestamp, status, timestep_max, wall_time_output, cpu_time_output, thread_count)})

    def populateList(self):
        self.list.InsertColumn(0, 'commit id', width=150)
        self.list.InsertColumn(1, 'commit message', width=150)
        self.list.InsertColumn(2, 'track notes', width=150)
        self.list.InsertColumn(3, 'relative time', width=150)
        self.list.InsertColumn(4, 'status', width=150)
        self.list.InsertColumn(5, 'timestep max', width=150)
        self.list.InsertColumn(6, 'total wall time (hr)', width=150)
        self.list.InsertColumn(7, 'total cpu time (hr)', width=150)
        self.list.InsertColumn(8, 'threads', width=150)

        items = self.archive_data.items()

        index = 0
        for key, data in items:
            self.list.InsertStringItem(index, data[0])           
            self.list.SetStringItem(index, 1, data[1])
            self.list.SetStringItem(index, 2, data[2])
            self.list.SetStringItem(index, 3, data[3])
            self.list.SetStringItem(index, 4, data[4])
            self.list.SetStringItem(index, 5, data[5])
            self.list.SetStringItem(index, 6, data[6])
            self.list.SetStringItem(index, 7, data[7])
            self.list.SetStringItem(index, 8, data[8])
            self.list.SetItemData(index, key)
            index += 1
        self.list.SetColumnWidth( 0, wx.LIST_AUTOSIZE ) 
        self.list.SetColumnWidth( 1, wx.LIST_AUTOSIZE ) 
        self.list.SetColumnWidth( 4, wx.LIST_AUTOSIZE ) 
   
    def GetListCtrl(self):
        return self.list
 
    def OnColClick(self, event):
        event.Skip()
        pass



class SingleSelectListCtrl( wx.ListCtrl, listmix.ListCtrlAutoWidthMixin ) :

    def __init__( self, parent, ID, axis, pos=wx.DefaultPosition,
              size=wx.DefaultSize, style=0 ) :
        
        wx.ListCtrl.__init__( self, parent, ID, pos, size, style )
        listmix.ListCtrlAutoWidthMixin.__init__( self )
        self.Bind(wx.EVT_LIST_ITEM_ACTIVATED, self.on_activation)

        self.cached_x1_active = frame.x1.list.active
        self.cached_y1_active = frame.y1.list.active
        self.cached_y2_active = frame.y2.list.active

    def on_activation(self, event):
        global global_commit
        index = event.GetIndex()
        item = frame.selector.available.list.GetItem(index,col=0) # col corresponds to the column where the commits ids are

        commit_string = item.GetText()
        path = os.path.join('.archive',commit_string,'output','output_step.csv')

        if (not (os.path.isfile(path))):
            wx.MessageBox('output_step.csv does not yet exist in {}'.format(path), 'Info', 
            wx.OK | wx.ICON_INFORMATION)
        else:
    # uncheck relevant items
            for item in frame.x1.list.active:
                frame.x1.list.CheckItem(data.inverted[item]-1, False)
            for item in frame.y1.list.active:
                frame.y1.list.CheckItem(data.inverted[item]-1, False)
            for item in frame.y2.list.active:
                frame.y2.list.CheckItem(data.inverted[item]-1, False)
            
            
            global_commit = commit_string
    
            # change plot title
    
            data.change_dataset(path)
    
    # stop any refreshing
            frame.force_stop_refresh()
    
    # clear the plot
            #frame.plot.axis.clear()
            #frame.plot.axis2.clear()
            frame.plot.canvas.draw()
    
    # clear the old checkbox lists
            frame.x1.list.clear()
            frame.y1.list.clear()
            frame.y2.list.clear()
    
    # make new ones
            frame.x1.layout(data)
            frame.y1.layout(data)
            frame.y2.layout(data)
    
    # this will set the correct length for the list (needed if the total number of output variables changes between archived simulations)
            frame.x1.run_set_count()
            frame.y1.run_set_count()
            frame.y2.run_set_count()


    # if the previouosly checked variable is present in the newly loaded dataset, then check it
            for old_var in self.cached_x1_active:
                if old_var in frame.x1.list.ordered_variables:
                    frame.x1.list.CheckItem(data.inverted[old_var]-1, True)

            for old_var in self.cached_y1_active:
                if old_var in frame.y1.list.ordered_variables:
                    frame.y1.list.CheckItem(data.inverted[old_var]-1, True)

            for old_var in self.cached_y2_active:
                if old_var in frame.y2.list.ordered_variables:
                    frame.y2.list.CheckItem(data.inverted[old_var]-1, True)

            frame.plot.update_plot(log_options=frame.log_options, axis_limits=frame.axis_limits)

            # leave the frame open
            #frame.selector.Destroy() 
        
if __name__ == "__main__":

    load_blank = 1
    blank_csv = StringIO('click "Change dataset"') # the simplest csv file
    
    # process command line 'blank_csv'
    this_script = sys.argv[0]

    if (len(sys.argv) > 2):
        print "ERROR: incorrect syntax for {}".format(this_script)
        print "\tuse either"
        print "\t\ttrack plot"
        print "\t\ttrack plot <commit-id>"
        sys.exit()
    if (len(sys.argv) == 2):
        load_blank = 0
        target_from_command_line = sys.argv[1]
        match = re.search('output_step.csv',target_from_command_line)
        if match:
            to_test = os.path.join(target_from_command_line)
            if (os.path.isfile(to_test)):
                print 'INFO: loading {}'.format(target_from_command_line)
                specified_step_file = target_from_command_line
            else:
                print "INFO: Nothing to plot, this is where I looked:\n\t./{0}".format(to_test)
                sys.exit()
        else:
            to_test = os.path.join(target_from_command_line,'output_step.csv')
            if (os.path.isfile(to_test)):
                specified_step_file = to_test
                print "INFO: loading ./{}".format(specified_step_file)
            else:
                print "INFO: Nothing to plot, this is where I looked:\n\t./{0}".format(to_test)
                sys.exit()
    elif (len(sys.argv) == 1):
        default_data = 'output/output_step.csv'
        if (os.path.isfile(default_data)):
            load_blank = 0
            specified_step_file = default_data
            print "INFO: loading {}".format(default_data)
        else:
            local_step_file='output_step.csv'
            if (os.path.isfile(local_step_file)):
                load_blank = 0
                specified_step_file = local_step_file
                print "INFO: loading ./{}".format(local_step_file)
            else:
                specified_step_file = 'blank_csv' # as a last resort
                print "INFO: Nothing to plot, this is where I looked:\n\t./{0}\n\t./output/{0}".format(local_step_file)
                sys.exit() # NOTE if this line is removed, then a dummy csv file is loaded

# previous method for loading archived step file
    #if (len(sys.argv) == 2):
    #    load_blank = 0
    #    specified_commit = sys.argv[1]
    #    global_commit = specified_commit
    #    print "INFO: {} called using commit {}".format(this_script, specified_commit)
    #    specified_step_file = os.path.join(run_archive,specified_commit,'output','output_step.csv')
    #elif (len(sys.argv) == 1):
    #    default_data = 'output/output_step.csv'
    #    if (os.path.isfile(default_data)):
    #        load_blank = 0
    #        specified_step_file = default_data
    #    else:
    #        specified_step_file = 'blank_csv' # as a last resort

    data = Data(specified_step_file)
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
