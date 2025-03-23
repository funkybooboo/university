#!/usr/bin/python3  	    	       

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

# python -m pip install --user -r requirements.txt  	    	       
from bs4 import BeautifulSoup  	    	       
from urllib.parse import urlparse, urljoin, urldefrag  	    	       
import requests  	    	       
import sys  	    	       
import time


def crawl(url, maxDepth, urls, depth):
    if maxDepth < 1:
        return
    urls.add(url)
    numSpaces = 4
    tab = numSpaces*depth*" "
    print(tab + url)
    try:
        html = requests.get(url).text
        soup = BeautifulSoup(html, 'html.parser')
        links = soup.find_all('a', href=True)
        if links:
            for link in links:
                joinedLink = urljoin(url, link["href"])
                goodLink = joinedLink.split('#')[0]
                if ('http' in goodLink or 'https' in goodLink) and not goodLink in urls:
                    crawl(goodLink, maxDepth-1, urls, depth+1)
        else:
            return
    except Exception as e:
        print(f"An exception occurred during link extraction: {e}")
    return

# If the crawler.py module is loaded as the main module, allow our `crawl` function to be used as a command line tool  	    	       
if __name__ == "__main__":  	    	       
    url = ''
    ## If no arguments are given...  	    	       
    if len(sys.argv) < 2:
        print("Please specify a url. See user manual for more information.")
        exit(1)
    else:  	    	       
        url = sys.argv[1]
    if (not 'http' in url and not 'https' in url) or not urlparse(url).netloc:
        print("Invalid url. See user manual for more information.")
        exit(1)
    maxDepth = 3
    if len(sys.argv) >= 3:
        try:
            maxDepth = int(sys.argv[2])
            if maxDepth < 1:
                raise Exception
        except Exception as e:
            print(f"Program crashed because invalid second argument. See user manual for more info.")
            exit(1)
    plural = 's' if maxDepth != 1 else ''
    url = url.split('#')[0]
    print(f"Crawling from {url} to a maximum depth of {maxDepth} link{plural}")
    startTime = time.time()
    urls = set()
    try:
        crawl(url, maxDepth, urls, 1)
    except KeyboardInterrupt as e:
        print(f"Program crashed because there was a keyboard interruption")
    except Exception as e:
        print(f"program crashed because {e}")
    endTime = time.time()
    timeTook = endTime - startTime
    print(f"Program took {'{:.2f}'.format(timeTook)} seconds to run. Number of unique urls visited: {len(urls)}")
