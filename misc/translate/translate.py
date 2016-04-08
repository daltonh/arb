#!/usr/bin/env python
# Python 2.7.3 --  64-bit

# script to translate any region in an output.*.msh file
# loops over all output.*.msh files in current directory
# creates corresponding files called translated.*.msh

# import pandas as pd
import re
import glob

import sys

import argparse
parser = argparse.ArgumentParser(description="Translate mesh")
parser.add_argument("--single", help="use for output.msh only", action='store_true')
parser.add_argument("--ten", help="apply every tenth mesh", action='store_true')
parser.add_argument("--hundred", help="apply every hundredth mesh", action='store_true')
parser.add_argument("--thousand", help="apply every thousandth mesh", action='store_true')
parser.add_argument("--tenthousand", help="apply every ten-thousandth mesh", action='store_true')
parser.add_argument("--custom", help="apply to specific mesh")
args = parser.parse_args()

translation = {}
# add a dict of what is requested to be translated
# note that multiple entries can be added using syntax:
# translation["<arb region name one>"] = (x_translation, y_translation, z_translation)
# translation["<arb region name two>"] = (x_translation, y_translation, z_translation)
# translation["<arb region name three>"] = (x_translation, y_translation, z_translation)

# EXAMPLE: translate <domain> region vertically by 1 unit
# here, use the example case
# "examples/transient_flow_around_cylinder_with_species"
translation["<flow domain>"] = (0, 1, 0)


# note that more complex translations are possible:
# for example, the translation can be different for each timestep if added to the "main processing loop" below
# one approach that has been useful is to (i) extract the current timestep from the "msh_file" string using a regex,
# then (ii) lookup the corresponding timestep in output_step.csv and extract a required translation using pd.read_csv(),
# then (iii) updated the translation[] dict for each loop iteration

#msh_files = glob.glob(r'output.*.msh')
if args.single:
    msh_files = glob.glob(r'output.msh')
elif args.ten:
    msh_files = glob.glob(r'output.*0.msh')
elif args.hundred:
    msh_files = glob.glob(r'output.*00.msh')
elif args.thousand:
    msh_files = glob.glob(r'output.*000.msh')
elif args.tenthousand:
    msh_files = glob.glob(r'output.*0000.msh')
elif args.custom:
    n = args.custom
    msh_files = glob.glob(r'output.{}.msh'.format(n))
else:
    msh_files = glob.glob(r'output.*.msh') # search for all

if not args.single:
    timestep_number = re.compile(r'output.(\d+).msh')


# gmsh processing information
node_count_lookup = {}
node_count_lookup[1] = 2 # gmsh type 1 elements are 2-node lines
node_count_lookup[2] = 3 # gmsh type 2 elements are 3-node triangle
node_count_lookup[3] = 4 # gmsh type 3 elements are 4-node quads
node_count_lookup[15] = 1 # gmsh type 15 elements are 1-node nodes

# INFO: "main processing loop" is here
for msh_file in msh_files:
    print "INFO: working on {}".format(msh_file)
    
    if not args.single:
        timestep = timestep_number.match(msh_file).group(1)
        timestep = int(timestep)

    start_physical_names = re.compile(r'\$PhysicalNames')
    end_physical_names = re.compile(r'\$EndPhysicalNames')

    physical_first = re.compile(r'^(\d)\s')
    physical_second = re.compile(r'^\d\s+(\d+)\s')
    physical_third = re.compile(r'\d\s+\d+\s+"(.+)"')
    process_physical_names = False

    name = {}
    number = {}

    # build required dictionaries
    with open(msh_file, 'r') as f:
        for line in f:
            line = line.rstrip()
            if start_physical_names.match(line):
                process_physical_names = True
                continue
            if end_physical_names.match(line):
                process_physical_names = False
            if process_physical_names:
                if len(line.split()) > 1:
                    dimension = int(physical_first.search(line).group(1))
                    region_number = int(physical_second.search(line).group(1))
                    region_name = physical_third.search(line).group(1)
                    #print dimension, region_number, region_name # debug
                    name[region_number] = region_name
                    number[region_name] = region_number

    start_elements = re.compile(r'\$Elements')
    end_elements = re.compile(r'\$EndElements')
    process_elements = False

    # build list of nodes that need translating
    nodes_to_translate = {}
    for target_region in translation:
        nodes_to_translate[target_region] = []

    with open(msh_file, 'r') as f:
        for line in f:
            line = line.rstrip()
            if start_elements.match(line):
                process_elements = True
                continue
            if end_elements.match(line):
                process_elements = False
                break
            if process_elements:
                data = line.split()
                if len(data) > 1:
                    region_number = int(data[3])
                    element_type = int(data[1])
                    node_count = node_count_lookup[element_type]
                    if region_number > 0: # deal with elements not associated with a region
                        nodes = data[-node_count:]
                        for target_region in translation:
                            if name[region_number] == target_region:
                                nodes_to_translate[target_region].extend(nodes)
    
    # remove duplicate nodes from list
    for target_region in translation:
        nodes_to_translate[target_region] = list(set(nodes_to_translate[target_region]))


    # translate the nodes
    start_nodes = re.compile(r'\$Nodes')
    end_nodes = re.compile(r'\$EndNodes')
    process_nodes = False

    if args.single:
        translate_target = "translated.msh"
    else:
        translate_target = 'translated.{}.msh'.format(timestep)
    with open(msh_file, 'r') as f, open(translate_target, 'w') as n:
        for line in f:
            if start_nodes.match(line):
                process_nodes = True
                n.write(line)
                continue
            if end_nodes.match(line):
                process_nodes = False
                n.write(line)
                continue
            if process_nodes:
                if len(line.split()) == 1:
                    n.write(line)
                    continue
            if process_nodes:
                output_string = line
                node_number, node_x, node_y, node_z = line.split()
                for target_region in translation:
                    if node_number in nodes_to_translate[target_region]:
                        node_x_translated = str(float(node_x) + translation[target_region][0])
                        node_y_translated = str(float(node_y) + translation[target_region][1])
                        node_z_translated = str(float(node_z) + translation[target_region][2])
                        # doesn't match the original gmsh formatting exactly but should be good enough
                        output_string = "{}\t{}\t{}\t{}\n".format(node_number.rjust(8),node_x_translated.rjust(14),node_y_translated.rjust(14),node_z_translated.rjust(14))
                n.write(output_string)
            else:
                n.write(line)


