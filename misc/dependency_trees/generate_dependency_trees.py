#!/usr/bin/env python

# generates dependency trees for equation variables


# `pydot` can deal with "clusters" of graphs (i.e. put multiple graphs on the same page) but litters the dot file with `width` and `pos` info
# `graphviz` is lightweight but cannot deal with clusters

import re
import os
import sys

class variable():
    def __init__(self, var_centring, var_kind, var_name, var_expression, var_dependencies, var_absfilename):
        self.centring = var_centring
        self.kind = var_kind
        self.name = var_name
        self.expession = var_expression
        self.dependencies = var_dependencies
        self.absfilename = var_absfilename

import argparse

setup_dir = './output/setup_data/'
file_path = setup_dir + 'variable_list.arb'
target_dir = setup_dir + 'dependency_trees/'

# can we get file_path inside this description?
description = """Generate dependency trees for equation variables
Example usage:
    ./misc/dependency_trees/generate_dependency_trees.py
        Make trees for all EQUATION entries in ./output/setup_creation_run/variable_list.arb

    ./misc/dependency_trees/generate_dependency_trees.py "<equation_1>" "<derived_1>" "<derived_2>"
        Make trees for EQUATION variable <equation_1>, DERIVED variable <derived_1> and DERIVED variable <derived_2> in ./output/setup_creation_run/variable_list.arb
"""

parser = argparse.ArgumentParser(description)
parser.add_argument('variables', nargs='*', help='list of arb variables to parse')
args = parser.parse_args()
#print args.variables # debug

# build list of traced variables (this does not include regions)
variable_list = []
equations_list = []
lookup = {}

if not os.path.isfile(file_path):
    print "INFO: {} does not exist".format(file_path)
    sys.exit()

if not os.path.exists(target_dir):
    os.makedirs(target_dir)

with open(file_path) as f:
    for line in f:
        line = line.rstrip()

        arb_input = re.search(r'(.*?)_(.*?) (<.*?>).*"(.*)".*absfilename = (.*?):', line)
        if arb_input:
            var_centring, var_kind, var_name, var_expression, var_absfilename = arb_input.groups()           
            var_dependencies = re.findall(r'(<.*?>)', var_expression)
            variable_list.append(var_name)
            # build equations list here, as later dicts don't retain parsing order
            if var_kind == "EQUATION":
                 equations_list.append(var_name)
            a = variable(var_centring, var_kind, var_name, var_expression, var_dependencies, var_absfilename)
            lookup[var_name] = a

# remove region names from dependencies
for key in lookup.keys():
    to_remove = []
    var = lookup[key]
    lookup[key].dependencies = list(set(lookup[key].dependencies)) # remove duplicates


    # remove any variables that are not present in `variable_list.arb` (covers arb system variables, eg. `<facex[l=1]>`)
    for dep in var.dependencies: 
        if dep not in variable_list:
            to_remove.append(dep)
    for dep in to_remove:
        lookup[key].dependencies.remove(dep)


#for key in lookup.keys():
#    lookup[key].color_explicit = False

#import graphviz as gv
import pydot # use pydot as it supports clusters

color = {}
color["DERIVED"] = "black"
color["TRANSIENT"] = "gray"
color["LOCAL"] = "purple"

color["EQUATION"] = "red"

color["UNKNOWN"] = "orange"
color["CONSTANT"] = "yellow"


def find_dependencies(var, last_n):
    global n
    global node_list
    global force_explicit

    #print "find_dependencies()", var.kind, var.name, "with n = ", n, 'last_n = ', last_n # debug
    collected.append(var.name)
   
    if var.kind not in ['UNKNOWN', 'CONSTANT']: # search stops if the variable is an 'UNKNOWN' or 'CONSTANT'
        #print "has dependencies ", var.dependencies #debug
        for dep in var.dependencies:
            n+=1
            #print "creating node for {} with n = {}, last_n = {}".format(dep, n, last_n) # debug
            label = dep
           
            shape = 'rectangle'
            if lookup[dep].kind == "UNKNOWN":
                shape = 'ellipse'
            
            node_style = 'solid'
            if lookup[dep].name in collected:
                node_style = 'dashed'
            
            parent = node_list[last_n]
            
            explicit = False
            if lookup[dep].kind == "TRANSIENT":
                explicit = True

            if force_explicit[last_n]:
                explicit = True

            if explicit:
                color_setting = "gray"
            else:
                color_setting = color[lookup[dep].kind]
            
            node_list[n] = lookup[dep].name
            force_explicit[n] = explicit # store for future reference

            tag = "{}_{}".format(cluster,n)
            last_tag = "{}_{}".format(cluster,last_n)

            #print "{}\t{}\t{}".format(tag, lookup[dep].kind, dep) #debug
            #print "\texplicit={}".format(explicit) #debug

            node = pydot.Node(tag, label=dep+" ", color=color_setting, shape=shape, style=node_style) # add " " space to avoid angle brackets at the beginning of the label string
            graph.add_node(node)
             
            #g.edge(last_tag, tag, constraint='true') # add connection from parent
            headlabel = lookup[dep].absfilename
            headlabel = headlabel.split(" ")[-1] # use the last path given in variable_list.arb
            parts = headlabel.split("/")
            headlabel = "/".join(parts[-2:])

            graph.add_edge(pydot.Edge(last_tag, tag, headlabel=headlabel, fontname="inconsolata", fontsize='8'))
            
            if lookup[dep].kind not in ['UNKNOWN', 'CONSTANT'] and lookup[dep].name not in collected:
                find_dependencies(lookup[dep], n)
            else:
                collected.append(dep)
    else:
        return

# show variables that are specified, otherwise show all equations
if args.variables:
    variables_to_show = args.variables
else:
    variables_to_show = equations_list

graph_list = []
cluster = 0
for var_to_show in list(variables_to_show):
    print "INFO: processing {}".format(var_to_show)
    var = lookup[var_to_show]
    
    # reset graph in loop
    graph = pydot.Dot(graph_type='digraph', rankdir="TB")
    #graph = pydot.Cluster(graph_name='{}'.format(cluster), fontname="inconsolata")
    graph.set_node_defaults(fontname="inconsolata", shape="rectangle")
    
    collected = []
    n = 0
    node_list = {}
    force_explicit = {}

    node = pydot.Node("{}_{}".format(cluster,n), label=" "+var.name)
    graph.add_node(node)
    
    node_list[n] = var.name
    force_explicit[n] = False

    find_dependencies(var, n)
    #graph_list.append(graph)

    pdf_name = var_to_show.strip("<").strip(">")
    pdf_name = re.sub(r"\s+", "_", pdf_name)

    #graph.write_pdf('cluster_{}.pdf'.format(cluster))
    filename = target_dir+'tree_{}_{}.pdf'.format(str(cluster).zfill(2), pdf_name)
    print "\t{}".format(filename)
    graph.write_pdf(filename) # pad with zeros

    cluster = cluster+1

#base = pydot.Dot(graph_type='digraph', rankdir="TB")
#base.set_node_defaults(fontname="inconsolata", shape="rectangle")
#for subgraph in graph_list:
#    base.add_subgraph(subgraph)
#base.write_pdf('base.pdf')
#base.write_dot('base.dot')
