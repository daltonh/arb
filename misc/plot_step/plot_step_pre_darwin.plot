####################################################################################
# plot_step_post.pre follows
#
# file plot_step.plot
# contains gnuplot commands for plotting output.step files created from arb
# this file is generated via the misc/plot_step/plot_step.pl script
# pre and post parts of the file are sourced from
#   1) misc/plot_step/plot_step_pre.plot and
#   2) misc/plot_step/plot_step_post.plot 

#------------------------------
#set terminal postscript eps enhanced color
#set output "plotme.eps"
#set terminal postscript enhanced color
#set terminal postscript enhanced
#set terminal pdf colour enhanced fsize 12 size 20cm,16cm
#set terminal pdf colour enhanced fsize 12 size 20cm,16cm dashed
#set output "plot.pdf"
#set terminal aqua font "Sans" # this works nicely except that it doesn't make the window active
#set terminal wxt enhanced font "Sans" persist # a prettier option for linux
#set terminal x11 enhanced font "Sans" persist # clunky but works well and is fast - q quits
#set terminal x11 enhanced font "Sans" persist noraise # keeps window in the background on rereading
set terminal x11 enhanced font "Sans" persist noraise dashed # keeps window in the background on rereading
#set term pdfcairo font "sans,12"

# for csv files
set datafile separator ","

#set size 1.,1.
#set origin 0.,0.
#set multiplot

#set size 0.5,0.5
#set origin 0.,0.

set title 'arb results'

#set logscale xy

# set up default line styles, 24 in total
# pi is point skipping, ie, pi 10 is to skip every 10 points
# points will not be written unless set style data linespoints is used
set style line 1  lt 1 pt 4  lc rgb "blue"
set style line 2  lt 1 pt 5  lc rgb "orange"
set style line 3  lt 1 pt 6  lc rgb "purple"
set style line 4  lt 1 pt 7  lc rgb "turquoise"
set style line 5  lt 1 pt 8  lc rgb "dark-chartreuse"
set style line 6  lt 1 pt 9  lc rgb "dark-grey"
set style line 7  lt 1 pt 10 lc rgb "dark-red"
set style line 8  lt 1 pt 11 lc rgb "black"
set style line 9  lt 2 pt 4  lc rgb "blue"
set style line 10 lt 2 pt 5  lc rgb "orange"
set style line 11 lt 2 pt 6  lc rgb "purple"
set style line 12 lt 2 pt 7  lc rgb "turquoise"
set style line 13 lt 2 pt 8  lc rgb "dark-chartreuse"
set style line 14 lt 2 pt 9  lc rgb "dark-grey"
set style line 15 lt 2 pt 10 lc rgb "dark-red"
set style line 16 lt 2 pt 11 lc rgb "black"
set style line 17 lt 4 pt 4  lc rgb "blue"
set style line 18 lt 4 pt 5  lc rgb "orange"
set style line 19 lt 4 pt 6  lc rgb "purple"
set style line 20 lt 4 pt 7  lc rgb "turquoise"
set style line 21 lt 4 pt 8  lc rgb "dark-chartreuse"
set style line 22 lt 4 pt 9  lc rgb "dark-grey"
set style line 23 lt 4 pt 10 lc rgb "dark-red"
set style line 24 lt 4 pt 11 lc rgb "black"

#set style data linespoints
set style data lines

#show palette colornames
# There are 112 predefined color names:
#   white              #ffffff = 255 255 255
#   black              #000000 =   0   0   0
#   dark-grey          #a0a0a0 = 160 160 160
#   red                #ff0000 = 255   0   0
#   web-green          #00c000 =   0 192   0
#   web-blue           #0080ff =   0 128 255
#   dark-magenta       #c000ff = 192   0 255
#   dark-cyan          #00eeee =   0 238 238
#   dark-orange        #c04000 = 192  64   0
#   dark-yellow        #c8c800 = 200 200   0
#   royalblue          #4169e1 =  65 105 225
#   goldenrod          #ffc020 = 255 192  32
#   dark-spring-green  #008040 =   0 128  64
#   purple             #c080ff = 192 128 255
#   steelblue          #306080 =  48  96 128
#   dark-red           #8b0000 = 139   0   0
#   dark-chartreuse    #408000 =  64 128   0
#   orchid             #ff80ff = 255 128 255
#   aquamarine         #7fffd4 = 127 255 212
#   brown              #a52a2a = 165  42  42
#   yellow             #ffff00 = 255 255   0
#   turquoise          #40e0d0 =  64 224 208
#   grey0              #000000 =   0   0   0
#   grey10             #1a1a1a =  26  26  26
#   grey20             #333333 =  51  51  51
#   grey30             #4d4d4d =  77  77  77
#   grey40             #666666 = 102 102 102
#   grey50             #7f7f7f = 127 127 127
#   grey60             #999999 = 153 153 153
#   grey70             #b3b3b3 = 179 179 179
#   grey               #c0c0c0 = 192 192 192
#   grey80             #cccccc = 204 204 204
#   grey90             #e5e5e5 = 229 229 229
#   grey100            #ffffff = 255 255 255
#   light-red          #f03232 = 240  50  50
#   light-green        #90ee90 = 144 238 144
#   light-blue         #add8e6 = 173 216 230
#   light-magenta      #f055f0 = 240  85 240
#   light-cyan         #e0ffff = 224 255 255
#   light-goldenrod    #eedd82 = 238 221 130
#   light-pink         #ffb6c1 = 255 182 193
#   light-turquoise    #afeeee = 175 238 238
#   gold               #ffd700 = 255 215   0
#   green              #00ff00 =   0 255   0
#   dark-green         #006400 =   0 100   0
#   spring-green       #00ff7f =   0 255 127
#   forest-green       #228b22 =  34 139  34
#   sea-green          #2e8b57 =  46 139  87
#   blue               #0000ff =   0   0 255
#   dark-blue          #00008b =   0   0 139
#   midnight-blue      #191970 =  25  25 112
#   navy               #000080 =   0   0 128
#   medium-blue        #0000cd =   0   0 205
#   skyblue            #87ceeb = 135 206 235
#   cyan               #00ffff =   0 255 255
#   magenta            #ff00ff = 255   0 255
#   dark-turquoise     #00ced1 =   0 206 209
#   dark-pink          #ff1493 = 255  20 147
#   coral              #ff7f50 = 255 127  80
#   light-coral        #f08080 = 240 128 128
#   orange-red         #ff4500 = 255  69   0
#   salmon             #fa8072 = 250 128 114
#   dark-salmon        #e9967a = 233 150 122
#   khaki              #f0e68c = 240 230 140
#   dark-khaki         #bdb76b = 189 183 107
#   dark-goldenrod     #b8860b = 184 134  11
#   beige              #f5f5dc = 245 245 220
#   olive              #a08020 = 160 128  32
#   orange             #ffa500 = 255 165   0
#   violet             #ee82ee = 238 130 238
#   dark-violet        #9400d3 = 148   0 211
#   plum               #dda0dd = 221 160 221
#   dark-plum          #905040 = 144  80  64
#   dark-olivegreen    #556b2f =  85 107  47
#   orangered4         #801400 = 128  20   0
#   brown4             #801414 = 128  20  20
#   sienna4            #804014 = 128  64  20
#   orchid4            #804080 = 128  64 128
#   mediumpurple3      #8060c0 = 128  96 192
#   slateblue1         #8060ff = 128  96 255
#   yellow4            #808000 = 128 128   0
#   sienna1            #ff8040 = 255 128  64
#   tan1               #ffa040 = 255 160  64
#   sandybrown         #ffa060 = 255 160  96
#   light-salmon       #ffa070 = 255 160 112
#   pink               #ffc0c0 = 255 192 192
#   khaki1             #ffff80 = 255 255 128
#   lemonchiffon       #ffffc0 = 255 255 192
#   bisque             #cdb79e = 205 183 158
#   honeydew           #f0fff0 = 240 255 240
#   slategrey          #a0b6cd = 160 182 205
#   seagreen           #c1ffc1 = 193 255 193
#   antiquewhite       #cdc0b0 = 205 192 176
#   chartreuse         #7cff40 = 124 255  64
#   greenyellow        #a0ff20 = 160 255  32
#   gray               #bebebe = 190 190 190
#   light-gray         #d3d3d3 = 211 211 211
#   light-grey         #d3d3d3 = 211 211 211
#   dark-gray          #a0a0a0 = 160 160 160
#   slategray          #a0b6cd = 160 182 205
#   gray0              #000000 =   0   0   0
#   gray10             #1a1a1a =  26  26  26
#   gray20             #333333 =  51  51  51
#   gray30             #4d4d4d =  77  77  77
#   gray40             #666666 = 102 102 102
#   gray50             #7f7f7f = 127 127 127
#   gray60             #999999 = 153 153 153
#   gray70             #b3b3b3 = 179 179 179
#   gray80             #cccccc = 204 204 204
#   gray90             #e5e5e5 = 229 229 229
#   gray100            #ffffff = 255 255 255

####################################################################################
