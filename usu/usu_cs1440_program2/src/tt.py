#!/usr/bin/env python  	    	       

#                         _  	    	       
#                        (o)<  DuckieCorp Software License  	    	       
#                   .____//  	    	       
#                    \ <' )   Copyright (c) 2022 Erik Falor  	    	       
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  	    	       
#  	    	       
# Permission is granted, to any person who is EITHER an employee OR  	    	       
# customer of DuckieCorp, to deal in the Software without restriction,  	    	       
# including without limitation the rights to use, copy, modify, merge,  	    	       
# publish, distribute, sublicense, and/or sell copies of the Software, and to  	    	       
# permit persons to whom the Software is furnished to do so, subject to the  	    	       
# following conditions:  	    	       
#  	    	       
# The above copyright notice and this permission notice shall be included in  	    	       
# all copies or substantial portions of the Software.  	    	       
#  	    	       
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR  	    	       
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  	    	       
# FITNESS FOR A PARTICULAR PURPOSE, EDUCATIONAL VALUE AND NONINFRINGEMENT. IN  	    	       
# NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,  	    	       
# DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR  	    	       
# OTHERWISE, ARISING FROM INDIGNATION, INDIGESTION, INDIFFERENCE, INDECENCY,  	    	       
# INDENTATION, INDETERMINATION, INTOXICATION, INDOCTRINATION, INTOLERANCE,  	    	       
# INDULGENCE, INDELICATENESS, INDISCRETION, INEFFECTIVENESS OR IN CONNECTION  	    	       
# WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.  	    	       


import sys

from Concatenate import cat, tac  	    	       
from CutPaste import cut, paste  	    	       
from Grep import grep  	    	       
from Partial import head, tail  	    	       
from Sorting import sort  	    	       
from WordCount import wc  	    	       
from Usage import usage
from Helper import helper

# 0 = tt.py
# 1 = tool i.e. cat or head

if len(sys.argv) < 2:  	    	       
    helper()
else:
    if sys.argv[1] == 'cat':
        if len(sys.argv[1:]) < 1:
            usage('Too few arguments', 'cat')
        cat(sys.argv[2:])
    elif sys.argv[1] == 'tac':
        if len(sys.argv[1:]) < 1:
            usage('Too few arguments', 'tac')
        tac(sys.argv[2:])
    elif sys.argv[1] == 'paste':
        if len(sys.argv[1:]) < 1:
            usage('Too few arguments', 'paste')
        paste(sys.argv[2:])
    elif sys.argv[1] == 'cut':
        if len(sys.argv[1:]) < 1:
            usage('Too few arguments', 'cut')
        cut(sys.argv[2:])
    elif sys.argv[1] == 'grep':
        if len(sys.argv[1:]) < 1:
            usage('Too few arguments', 'grep')
        grep(sys.argv[2:])
    elif sys.argv[1] == 'head':
        if len(sys.argv[2:]) < 1:
            usage('Too few arguments', 'head')
        head(sys.argv[2:])
    elif sys.argv[1] == 'tail':
        if len(sys.argv[1:]) < 1:
            usage('Too few arguments', 'tail')
        tail(sys.argv[2:])
    elif sys.argv[1] == 'sort':
        if len(sys.argv[1:]) < 1:
            usage('Too few arguments', 'sort')
        sort(sys.argv[2:])
    elif sys.argv[1] == 'wc':
        if len(sys.argv[1:]) < 1:
            usage('Too few arguments', 'wc')
        wc(sys.argv[2:])
    else:
        helper()
