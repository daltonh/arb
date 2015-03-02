#!/usr/bin/env python
from __future__ import division
import pandas as pd
import matplotlib.pyplot as plt
import re
import sys

this_script = sys.argv[0]
if (len(sys.argv) == 1):
    print "INFO: usage for {}".format(this_script)
    print "\t./plot_timings <option>"
    print "\tvalues for <option>:"
    print "\t1 =>  total (cumulative) update time"
    print "\t2 =>  relative total update time"
    print "\t3 =>  total updates"
    print "\t4 =>  average (per update) update time"
    print "\t5 =>  relative average update time"
    sys.exit()

if (len(sys.argv) == 2):
    timing_type_index = int(sys.argv[1])
    timing_label = ''

def data_line(line):
    if(re.match(r'#', line)):
        return 0
    else:
        return 1

def nonblank_line(f):
    for l in f:
        line = l.rstrip()
        if line:
            yield line

holder = {}
with open('./output/output.stat') as stat:
    for line in nonblank_line(stat):
        if (data_line(line)):
            entries = line.split(':')
            variable_name = entries[0]
            
            timing = entries[timing_type_index-6] # the timings are at the end of the list, so read from the end inwards
            timing_type = timing.split('=')[0].strip()
            timing_value = float(timing.split('=')[1].strip()) # for example, turn "total (cumulative) update time =  0.11718750" into "0.11718750"
            holder[variable_name] = timing_value
            timing_label = timing_type

df = pd.DataFrame(holder.items(), columns=['variable', 'timing'])
df.sort('timing', inplace=True)

new = df # to plot all
#new = df[df['timing']>100.] # to only plot timings above certain value

def make_plot():
    fig = plt.figure(figsize=(10,len(new)/10+10))
    ax = fig.add_subplot(1,1,1)
    ax.barh(range(len(new["timing"])),
                     new["timing"],
                     height=0.8,
                     align="center",
                     color="#8A0707",
                     edgecolor="none",
    #                 log=True,
                     )
    
    num = len(new["timing"])
    plt.yticks(range(num), new["variable"].values, fontsize=7)
    plt.ylim([-1,num])
    #plt.xlabel('relative total update time')
    plt.xlabel(timing_label)
    #xticks(arange(0, 5, 1), [""])
    ax.xaxis.set_label_position("top")
    ax.xaxis.tick_top()
    
print df.to_string(index=False)

plot_name='timings.pdf'
print "INFO: values above represent '{}'".format(timing_label)
print "INFO: making plot '{}'".format(plot_name)

make_plot()
#plt.show()
plt.savefig(plot_name, bbox_inches='tight')


