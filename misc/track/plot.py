#!/usr/bin/env python

# user interface for visualising contents of output/output_step.csv
# requires additional python libraries which can be installed following these steps

# ubuntu:
# run the script "misc/track/install_plot_dependencies_on_ubuntu.sh"

# osx:
# run the script "misc/install_dependencies/install_dependencies_on_osx_using_macports.sh"
# otherwise use a python install such as 
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

import argparse
from collections import defaultdict

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
legend_font_size=9

class Data():
    show_markers = True # class attribute
    show_lines = True # class attribute
    show_legend = True # class attribute

    def __init__(self, step_file_path, summary_file=False):
        # summary_file=True implies file of type batcher_output.csv
        # summary_file=False (default) implies file of type output_step.csv

        # read output_step.csv into pandas data frame
        # ignore the row of dimension strings ([1:])
        # (in future, could use dimension strings to automatically show dimensions in plot labels)
        #self.step_file_path = 'output/output_step.csv'
        self.step_file_path = step_file_path
        self.summary_file = summary_file
        self.show_newtsteps = False
        self.converged_data_present = True

        self.open_data()
        self.process_data()

        self.path_tag = ''

    def open_data(self):
        global load_blank

        if (load_blank):
            self.step_file = blank_csv
        elif (os.path.isfile(self.step_file_path)):
            self.step_file = open(self.step_file_path)
        else:
            print "ERROR: {} does not exist".format(self.step_file_path)
            sys.exit() 
        
    def process_data(self):

        global load_blank
        if (load_blank):
            self.df=pd.read_csv(blank_csv, comment='#')[1:]
        else:
            # batch_data.csv files are small, read into string and replace midline
            # '#' comment characters that are used in <<comment>> replacements within arb
            if self.summary_file:
                # store in string
                with open(self.step_file_path, 'r') as f:
                    data=f.read()
                data = re.sub('# run', '"run"', data)
                data = re.sub(re.compile(r'(?<!^)#',re.MULTILINE),'NA', data) # replace all non-comment hashes with NA, this is an artifact of using eg R "<<varcomment>>" W "#" in arb
                ready_for_input = StringIO(data)

                # assemble data frame 
                try:
                    self.df = pd.read_csv(ready_for_input, comment='#')
                except:
                    sys.exit("ERROR: problem opening {}".format(self.step_file_path))
                if self.df.empty:
                    print "INFO: {} contains no converged data".format(self.step_file_path)
                    self.converged_data_present = False
                    return # leave early, we won't do anything else with the data is converged_data_present is False

                filtered = []
                for item in self.df.columns:
                    filtered.append(item.rstrip('\"').lstrip(' \"'))

                self.df.columns = filtered
                self.df.sort(['run'],inplace=True) # sort by run with parallel jobs are out of order
            else:
                # extra row for arb variable dimensions
                self.df=pd.read_csv(self.step_file, comment='#', **read_csv_options)[1:]
        load_blank = 0
        # convert all but header labels to float values

        if not self.summary_file:
            self.df = self.df.astype(float)
            if (not self.show_newtsteps and not load_blank):
                tmp_variable = "<timestep>"
                try:
                    self.df = self.df.groupby(tmp_variable, as_index=False).nth(-1)
                except:
                    pass
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

    def __init__(self, parent, data_object_list, axis):

        wx.Panel.__init__( self, parent=parent, id=wx.ID_ANY )
        self.axis = axis
        self.create(data_object_list)
        self.layout(data_object_list)
        self.run_set_count()
        
    def create(self, data_object_list):
        self.sizer = wx.BoxSizer( wx.VERTICAL )
        self.list = SortableListCtrl( self, wx.ID_ANY, axis=self.axis, style=wx.LC_REPORT
                             | wx.BORDER_NONE
                             | wx.LC_EDIT_LABELS
                             | wx.LC_SORT_ASCENDING, )

        self.sizer.Add(self.list, 1, wx.EXPAND)
        self.list.InsertColumn(0, axis[self.axis], width=150)

    def layout(self, data_object_list):
        self.populateList(data_object_list)
        self.itemDataMap = self.selection_stripped # when sorting the list, use the stripped variables
        listmix.ColumnSorterMixin.__init__(self, numColumns=1)
        self.SetSizer(self.sizer)
        self.SetAutoLayout(True)

    def run_set_count(self):
        self.list.set_count()
       
    def populateList(self, data_object_list):

        master = [] # contains all variables with repetition
        union = [] # contains all variables at most once

        self.selection = {} # what will be shown in the gui list
        self.selection_stripped = {}
        
        for data_set in data_object_list:
            master.extend(data_set.variables.values())

        for item in master:
            if item not in union:
                union.append(item)
        
        for i, variable in enumerate(union):
            self.selection[i+1] = variable
            chars = ("'", '<', '>')
            for char in chars:
                variable = variable.replace(char,'')
            self.selection_stripped[i+1] = variable

        for key, data in self.selection.items():
            index = self.list.InsertStringItem(sys.maxint, data)           
            self.list.SetItemData(index, key)
        self.list.SetColumnWidth( 0, wx.LIST_AUTOSIZE )
    
    def GetListCtrl(self) :
        return self.list


class CanvasPanel(wx.Panel):
    def __init__(self, parent, data_object_list):
        wx.Panel.__init__(self, parent)
        self.data_object_list = data_object_list
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



    def update_plot(self, log_options, axis_limits):

        # upon each update the canvas gets cleared
        self.axis.clear()
        self.axis2.clear()

        self.legendEntries1=[]
        self.legendEntries2=[]
        self.legendText1=[]
        self.legendText2=[]

        def variable_available(variable, data_object):
            available = True
            if variable not in data_object.inverted.keys():
                available = False
            return available

        def update_y1_var(y1_var):
            for data_object in self.data_object_list:
                if variable_available(y1_var, data_object):
                    current1, = self.axis.plot(
                            data_object.df[self.x1_var].values,
                            data_object.df[y1_var].values,
                            marker=marker_y1,
                            linestyle=line_y1,
                            markevery=point_interval,
                            alpha = y1_alpha,
                            rasterized=rasterized_option
                            )
                    self.legendEntries1.append(current1)
                    y1_legend_entry = y1_var.replace("'",'')
                    if data_object.path_tag:
                        y1_legend_entry = y1_legend_entry + ", {}".format(data_object.path_tag)

                    self.legendText1.append(y1_legend_entry)
                    
                    # extend axis range, if we need to
                    requested_x1_min = min(data_object.df[self.x1_var].min(),self.axis.get_xlim()[0])
                    requested_x1_max = max(data_object.df[self.x1_var].max(),self.axis.get_xlim()[1])                   
                    self.axis.set_xlim(requested_x1_min,requested_x1_max)
                    text1 = self.legendText1
                    entries1 = self.legendEntries1
                    if Data.show_legend:
                        lgd1 = self.axis.legend(
                                entries1,
                                text1,
                                numpoints=1, 
                                loc='upper left',
                                prop={'size':legend_font_size}
                                )

        def update_y2_var(y2_var):
            for data_object in self.data_object_list:
                if variable_available(y2_var, data_object):                
                    current2, = self.axis2.plot(
                            data_object.df[self.x1_var].values,
                            data_object.df[y2_var].values,
                            marker=marker_y2,
                            linestyle=line_y2,
                            markevery=point_interval,
                            alpha = y2_alpha,
                            rasterized=rasterized_option
                            )
                    #self.axis2.set_xlim(data_object.df[self.x1_var].min(),data_object.df[self.x1_var].max())                    
                    self.legendEntries2.append(current2)
                    y2_legend_entry = y2_var.replace("'",'')
                    if data_object.path_tag:
                        y2_legend_entry = y2_legend_entry + ", {}".format(data_object.path_tag)
                    self.legendText2.append(y2_legend_entry)
                    text2 = self.legendText2
                    entries2 = self.legendEntries2
                    if Data.show_legend:
                        lgd2 = self.axis2.legend(
                                entries2,
                                text2,
                                numpoints=1, 
                                loc='upper right',
                                prop={'size':legend_font_size}
                                )
        # deal with x1_var
        if (frame.x1.list.active_count == 1):
            # set x1_var
            self.x1_var = frame.x1.list.active[0]

            # deal with possible x1 log scale
            if (log_options[0]):
                self.axis.set_xscale('log')
            else:
                self.axis.ticklabel_format(useOffset=False)

            point_interval = frame.point_interval_widget.GetValue()
            marker_y1 = None
            marker_y2 = None
            line_y1 = 'None'
            line_y2 = 'None'
            if (Data.show_markers):
                marker_y1 = 'o'
                marker_y2 = '^'
            if (Data.show_lines):
                line_y1 = '-'
                line_y2 = '-'
            
            # set color cycles before variable loops
            self.axis.set_color_cycle(list1)
            self.axis2.set_color_cycle(list2)

            # loop over active y1_var
            if (frame.y1.list.active_count > 0):           
                if (log_options[1]):
                    self.axis.set_yscale('log')
                for y1_var in frame.y1.list.active:
                    update_y1_var(y1_var)
            else:
                self.axis.clear()
            self.axis.set_xlabel(self.x1_var.replace("'",''))

            # loop over active y2_var
            if (frame.y2.list.active_count > 0):           
                if (log_options[2]):
                    self.axis2.set_yscale('log')
                else:
                    if not log_options[0]:
                        self.axis2.ticklabel_format(useOffset=False)
                   
                for y2_var in frame.y2.list.active:
                    update_y2_var(y2_var)
            else:
                self.axis2.clear()
                self.axis2.axis('off')

        else: # if no (single) appropriate x1_var is selected then clear
                self.axis.clear()

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
 
    def __init__(self, parent, id, title, data_object_list):
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

        self.data_object_list = data_objects
       
        container_panel_left = wx.Panel(self, -1)        
        self.x1 = AxisPanel(container_panel_left, self.data_object_list, axis=0)
        self.y1 = AxisPanel(container_panel_left, self.data_object_list, axis=1)
        self.y2 = AxisPanel(container_panel_left, self.data_object_list, axis=2)
        
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

        self.show_legend_box = wx.CheckBox(container_panel_options, label='Show legend', pos=(15, 20))
        self.show_legend_box.SetValue(True)
        self.show_markers_box = wx.CheckBox(container_panel_options, label='Show markers', pos=(15, 20))
        self.show_markers_box.SetValue(True)
        self.show_lines_box = wx.CheckBox(container_panel_options, label='Show lines', pos=(15, 20))
        self.show_lines_box.SetValue(True)

        self.show_newtsteps_box = wx.CheckBox(container_panel_options, label='Show newtsteps', pos=(15, 20))

        set_point_interval_sizer.Add(self.point_interval_widget, proportion=0, flag=wx.ALL, border=1)
        set_point_interval_sizer.Add(self.show_markers_box, proportion=0, flag=wx.ALL, border=1)
        set_point_interval_sizer.Add(self.show_lines_box, proportion=0, flag=wx.ALL, border=1)
        set_point_interval_sizer.Add(self.show_legend_box, proportion=0, flag=wx.ALL, border=1)
        set_point_interval_sizer.Add(self.show_newtsteps_box, proportion=0, flag=wx.ALL, border=1)

        self.Bind( wx.EVT_SPINCTRL, self.OnSpin )
        if (operating_system == 'Darwin' and not old_wx_version):
            self.tmp_txtctrl.Bind(wx.EVT_TEXT_ENTER, self.OnSpin)
    
        self.show_legend_box.Bind(wx.EVT_CHECKBOX, self.ShowLegend)
        self.show_markers_box.Bind(wx.EVT_CHECKBOX, self.ShowMarkers)
        self.show_lines_box.Bind(wx.EVT_CHECKBOX, self.ShowLines)
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
        vbox_options.Add(set_log_sizer, proportion=0, flag=wx.TOP, border=1)
        vbox_options.Add(set_point_interval_sizer, proportion=0, flag=wx.TOP, border=1)
        vbox_options.Add(set_ranges_sizer, proportion=0, flag=wx.TOP, border=1)
        vbox_options.Add(matplotlib_export_sizer, proportion=0, flag=0, border=1)
        if (show_plot_step_export):
            vbox_options.Add(plot_step_export_sizer, proportion=0, flag=0, border=1)
        vbox_options.Add(refresh_sizer, proportion=0, flag=0, border=1)
        container_panel_options.SetSizer(vbox_options)

        container_panel_right = wx.Panel(self, -1)
        self.plot = CanvasPanel(container_panel_right, self.data_object_list)                                      

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
        Data.show_markers = self.show_markers_box.GetValue() # modify class attribute
        self.call_plot_upate()

    def ShowLines(self, event):
        Data.show_lines = self.show_lines_box.GetValue() # modify class attribute
        self.call_plot_upate()

    def ShowLegend(self, event):
        Data.show_legend = self.show_legend_box.GetValue() # modify class attribute
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

        t = msg.data
        if (isinstance(t, int) and not self.block):
            # if we receive an integer message then reload
            # re-read the data

            try:
                for data_object in self.data_object_list:
                    data_object.step_file = open(data_object.step_file_path)
                    data_object.df=pd.read_csv(data_object.step_file, comment='#', **data_object.read_csv_options)[1:]
                    # convert all but header labels to float values
                    data_object.df = data_object.df.astype(float)
                    data_object.step_file.close()
                    data_object.df = data_object.df.astype(float)
                self.call_plot_upate()                    
            except:
                pass
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

    def OnQuit(self, event):
        frame.selector.Close() 
        frame.selector.Destroy() 
        self.Close()
        self.Destroy()

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
        index = event.GetIndex()

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


    # if the previously checked variable is present in the newly loaded dataset, then check it
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

    def activate_default_x1_variable():
        # here we set a default x1 variable to be activated
        # for transient data we choose "<timestep>"
        # for steady-state data we choose "<newtstep>"
        # for batch data (batch_data.csv) we choose "run"

        if args.batcher:
            tmp_variables = ["run"]
        else: 
            tmp_variables = ["<timestep>", "<newtstep>"]

        for tmp_variable in tmp_variables:
            try:
                frame.x1.list.CheckItem(data.inverted[tmp_variable]-1, True)
                break
            except:
                pass

        # here we show some info about what happened on the command line
        if tmp_variable == "<timestep>":
            print "INFO: data is of transient type"
        elif tmp_variable == "<newtstep>":
            print "INFO: data is of steady-state type"
        elif tmp_variable == "run":
            print "INFO: data is of batch type"

    load_blank = 1
    blank_csv = StringIO('click "Change dataset"') # the simplest csv file
    
    # process command line 'blank_csv'
    this_script = sys.argv[0]

    parser = argparse.ArgumentParser(description="Plot script for output_step.csv files")
    parser.add_argument("source", nargs='*', default=None, help="Path to data file/s, or path to data directory/s containing file. Note you can specify multiple paths and wildcard expressions")
    parser.add_argument("-s","--show", 
            help='instructions regarding variables to show, example: ./plot output/output_step.csv -s "<timestep>:<t>,<dt>:<newtstep>" '
            )    
    parser.add_argument("-b","--batcher", action="store_true", default=False, 
            help='batcher mode: load batch_data.csv files instead of output_step.csv files'
            ) 
    parser.add_argument("-p","--previous", action="store_true", default=False, 
            help='previous mode: allow output_step.csv files from directories named "previous" to be shown'
            ) 
    args = parser.parse_args()


    # check if step_file was specified in command line arguments
    myargs = vars(args)

    def contains_strings(line):
        if args.batcher:
            if(re.match(r'# run', line)):
                return 1
            else:
                return 0
        else:
            if(re.match(r'#', line)):
                return 0
            else:
                return 1

    def set_csv_options(reference_file):
        with open(reference_file) as f:
            for line in f:
                if contains_strings(line):
                    if line[-2] == "\'": # look for the last character, note [-1] will be \n
                        found_quotechar = "'" # set csv quotechar to single quote
                    else:
                        found_quotechar = '"' # set csv quotechar to double quote
                    break
        f.close() # might be redundant
        return {'quotechar':found_quotechar, 'low_memory':False}

    def find_step_files(path):
        csv_list = []
        for root, dirs, files in os.walk(path):
                for file in files:
                    if file.endswith("output_step.csv"):
                        csv_list.append(os.path.join(root, file))
        return csv_list

    def find_batch_data_files(path):
        csv_list = []
        for root, dirs, files in os.walk(path):
                for f in files:
                    if f.endswith("batch_data.csv"):
                        csv_list.append(os.path.join(root, f))
        return csv_list
    
    def natural_sort_key(s, _nsre=re.compile('([0-9]+)')):
        return [int(text) if text.isdigit() else text.lower()
                for text in re.split(_nsre, s)]    

    data_files_to_show = [] # list of strings that are paths to data files
    data_objects = [] # the actual Data instances corresponding to these strings
    target_list = myargs['source'] # strings coming in from the command line

    found_step_file = False
    found_batch_data_file = False

    if target_list: # directories were specified from command line
        load_blank = 0        
        targets_from_command_line = args.source

        for target in targets_from_command_line:
            match_batch = re.search(r'batch_data.csv$',target)
            match_step = re.search(r'output_step.csv$',target)
            if (args.batcher and match_batch) or (not args.batcher and match_step): # path to csv file has been specified
                to_test = os.path.join(target)
                if (os.path.isfile(to_test)):
                        print 'INFO: loading {}'.format(target)
                        data_files_to_show.append(target)
                else:
                    print "ERROR: Nothing to plot, this is where I looked:\n\t./{0}".format(to_test)
                    sys.exit()
            else: # path to a directory has been specified
                if os.path.isdir(target):
                    print "INFO: searching recursively for all files in ./{}".format(target)
                if args.batcher:        
                   data_files = find_batch_data_files(target)
                else:
                   data_files = find_step_files(target)
                data_files = sorted(data_files, key=natural_sort_key)
                if data_files:
                    print "INFO: loading the following from ./{}".format(target) 
                    for data_file in data_files:
                        if not args.previous:
                            if re.search('previous', data_file):
                                print "\tNOTE: skipping {}, use --previous option to show".format(data_file)
                            else:
                                print "\t./{}".format(data_file)
                                data_files_to_show.append(data_file)
                        else:
                            print "\t./{}".format(data_file)
                            data_files_to_show.append(data_file)
    
                elif os.path.isdir(target):
                    print "INFO: no files found in ./{}".format(target)

    else: # no directories were specified at command line, use default search for data files
        if args.batcher:
            default_data = 'batcher_output/batch_data.csv'
        else:
            default_data = 'output/output_step.csv'
        if (os.path.isfile(default_data)):
            load_blank = 0
            print "INFO: loading {}".format(default_data)
            data_files_to_show.append(default_data)
        else:
            if args.batcher:
                local_data_file='batch_data.csv'
            else:
                local_data_file='output_step.csv'
            if (os.path.isfile(local_data_file)):
                load_blank = 0
                print "INFO: loading ./{}".format(local_data_file)
                data_files_to_show.append(local_data_file)
            else:
                specified_data_file = 'blank_csv' # as a last resort
                print "INFO: Nothing to plot, this is where I looked:\n\t./{0}\n\t./output/{0}".format(local_data_file)
                sys.exit() # NOTE if this line is removed, then a dummy csv file is loaded

    for data_file in data_files_to_show:
        load_blank = 0
        # figure out whether single or double quotes are used in csv file
        read_csv_options = set_csv_options(data_file)
        if args.batcher:
            data = Data(data_file, summary_file=True)
        else:
            data = Data(data_file)
        data.path_tag = os.path.dirname(data_file)
        data.read_csv_options = set_csv_options(data_file) # keep a copy so that refresh works, note global read_csv_options got used on instantiation
        if data.converged_data_present:
            data_objects.append(data) # only one object in the list as we're not using batcher mode

    if data_objects:
        app = wx.App(False)
        directory_name = os.path.basename(os.getcwd())
        frame = FrameGenerator(None, -1, title=directory_name, data_object_list=data_objects)
    else:
        error_string = "ERROR: no valid data to show\n"
        error_string += "\tI looked in {}\n".format(args.source)
        error_string += '\tdid you mean to use `plot -s "<var>"`?'
        sys.exit(error_string)

 
    if args.show: # show variables based on command line arguments
        data = data_objects[0]
        print "INFO: showing variables based upon {}".format(args.show)
        axis_strings = args.show.split(":")
        n_specified_axes=len(axis_strings)
        for i, axis in enumerate(axis_strings):
            if args.batcher:
                variables = re.split(",",axis) # batch data args don't necessarily have angled bracket
            else:
                variables = re.split("(?<=[>]),",axis) # use a regex look behind, otherwise eg variable <var[l=1,r=2]> would get split
            for var in variables:
                if var: # deals with "<timestep>::<t>" type show string
                    try:
                        if (i==0 and n_specified_axes==1):
                            # show string is "<var1>,<var2>", so activate default variable on x1 axis, and the specified variables on the y1 axis
                            activate_default_x1_variable()
                            frame.y1.list.CheckItem(data.inverted[var]-1, True)
                        if (i==0 and n_specified_axes>1):
                            frame.x1.list.CheckItem(data.inverted[var]-1, True)
                        elif i==1:
                            frame.y1.list.CheckItem(data.inverted[var]-1, True)
                        elif i==2:
                            frame.y2.list.CheckItem(data.inverted[var]-1, True)
                    except:
                        pass
                        #sys.exit("{} does not contain variable {}".format(specified_step_file,var))   

    else:
        activate_default_x1_variable()

    frame.Show()

    app.MainLoop()



