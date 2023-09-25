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


import time  	    	       
import sys  	    	       
from Report import Report

def usage(message, errorCode):
    print("\n" + message)
    print("Usage: src/bigData.py DATA_DIRECTORY")
    sys.exit(errorCode)

def getDataFromAS(ASFileObj, dictionaryOfIndexes, dicOfFipsToNames):
    # for all industries
    numAreasAL = 0

    totalAnnualWagesAL = 0
    maxAnnualWageAL = 0
    maxAnnualWageCityAL = ""

    totalEstabAL = 0
    maxEstabAL = 0
    maxEstabCityAL = ""

    totalEmplAL = 0
    maxEmplAL = 0
    maxEmplCityAL = ""

    # for the software industry
    numAreasSI = 0

    totalAnnualWagesSI = 0
    maxAnnualWageSI = 0
    maxAnnualWageCitySI = ""

    totalEstabSI = 0
    maxEstabSI = 0
    maxEstabCitySI = ""

    totalEmplSI = 0
    maxEmplSI = 0
    maxEmplCitySI = ""

    while True:
        line = (ASFileObj.readline()).replace("\n","")
        if not line:
            break
        line = line.replace('"', "")
        stringList = line.split(",")
        fipsCode = stringList[dictionaryOfIndexes.get("area_fips")]
        if fipsCode[2] == "0" and fipsCode[3] == "0" and fipsCode[4] == "0":
            continue
        if fipsCode[0] == "U" and fipsCode[1] == "S":
            continue
        if fipsCode[0] == "C":
            continue
        if fipsCode[0] == "C" and fipsCode[1] == "S":
            continue
        if fipsCode[0] == "M":
            continue
        if fipsCode[0] == "C" and fipsCode[1] == "M" and fipsCode[2] == "S":
            continue

        # all industries
        if stringList[dictionaryOfIndexes.get("industry_code")] == "10" and stringList[dictionaryOfIndexes.get("own_code")] == "0":
            cityName = dicOfFipsToNames.get(stringList[dictionaryOfIndexes.get("area_fips")])
            numAreasAL += 1

            annualWagesAL = int(stringList[dictionaryOfIndexes.get("total_annual_wages")])
            if maxAnnualWageAL < annualWagesAL:
                maxAnnualWageAL = annualWagesAL
                maxAnnualWageCityAL = cityName
            totalAnnualWagesAL += annualWagesAL

            estabAL = int(stringList[dictionaryOfIndexes.get("annual_avg_estabs")])
            if maxEstabAL < estabAL:
                maxEstabAL = estabAL
                maxEstabCityAL = cityName
            totalEstabAL += estabAL

            emplAL = int(stringList[dictionaryOfIndexes.get("annual_avg_emplvl")])
            if maxEmplAL < emplAL:
                maxEmplAL = emplAL
                maxEmplCityAL = cityName
            totalEmplAL += emplAL

        # software comp
        elif stringList[dictionaryOfIndexes.get("industry_code")] == "5112" and stringList[dictionaryOfIndexes.get("own_code")] == "5":
            cityName = dicOfFipsToNames.get(stringList[dictionaryOfIndexes.get("area_fips")])
            numAreasSI += 1

            annualWagesSI = int(stringList[dictionaryOfIndexes.get("total_annual_wages")])
            if maxAnnualWageSI < annualWagesSI:
                maxAnnualWageSI = annualWagesSI
                maxAnnualWageCitySI = cityName
            totalAnnualWagesSI += annualWagesSI

            estabSI = int(stringList[dictionaryOfIndexes.get("annual_avg_estabs")])
            if maxEstabSI < estabSI:
                maxEstabSI = estabSI
                maxEstabCitySI = cityName
            totalEstabSI += estabSI

            emplSI = int(stringList[dictionaryOfIndexes.get("annual_avg_emplvl")])
            if maxEmplSI < emplSI:
                maxEmplSI = emplSI
                maxEmplCitySI = cityName
            totalEmplSI += emplSI

    return numAreasAL, totalAnnualWagesAL, maxAnnualWageAL, maxAnnualWageCityAL, totalEstabAL, maxEstabAL, maxEstabCityAL, totalEmplAL, maxEmplAL, maxEmplCityAL, numAreasSI, totalAnnualWagesSI, maxAnnualWageSI, maxAnnualWageCitySI, totalEstabSI, maxEstabSI, maxEstabCitySI, totalEmplSI, maxEmplSI, maxEmplCitySI

def getDicOfRowNamesToIndexes(ASFileObj):
    lineOne = (ASFileObj.readline()).replace("\n","")
    lineOne = lineOne.replace('"', "")
    stringList = lineOne.split(",")
    areaFipsIndex = 0
    industryCodeIndex = 0
    ownCodeIndex = 0
    totalAnnualWagesIndex = 0
    annualAvgEmplvlIndex = 0
    annualAvgEstabsIndex = 0
    for i in range(0,len(stringList)):
        if stringList[i] == "area_fips":
            areaFipsIndex = i
        elif stringList[i] == "industry_code":
            industryCodeIndex = i
        elif stringList[i] == "own_code":
            ownCodeIndex = i
        elif stringList[i] == "total_annual_wages":
            totalAnnualWagesIndex = i
        elif stringList[i] == "annual_avg_emplvl":
            annualAvgEmplvlIndex = i
        elif stringList[i] == "annual_avg_estabs":
            annualAvgEstabsIndex = i
    return {"area_fips" : areaFipsIndex, "industry_code" : industryCodeIndex, "own_code" : ownCodeIndex, "total_annual_wages" : totalAnnualWagesIndex, "annual_avg_emplvl" : annualAvgEmplvlIndex, "annual_avg_estabs" : annualAvgEstabsIndex}

def getRelivantFipsTooNames(ATFileObj):
    dictionary = {}
    areaFipsIndex = 0
    areaTitleIndex = 0
    lineOne = (ATFileObj.readline()).replace("\n","")
    lineOne = lineOne.replace('"', "")
    lineOne = lineOne.split(",")
    if lineOne[0] == "area_fips" and lineOne[1] == "area_title":
        areaFipsIndex = 0
        areaTitleIndex = 1
    elif lineOne[1] == "area_fips" and lineOne[0] == "area_title":
        areaFipsIndex = 1
        areaTitleIndex = 0
    else:
        usage("couldn't find the correct data in area-titles.csv", 8)
    while True:
        line = (ATFileObj.readline()).replace("\n","")
        if not line:
            break
        line = line.replace('"', "")
        line = line.split(",",maxsplit=1)
        fipsCode = line[areaFipsIndex]
        if fipsCode[2] == "0" and fipsCode[3] == "0" and fipsCode[4] == "0":
            continue
        if fipsCode[0] == "U" and fipsCode[1] == "S":
            continue
        if fipsCode[0] == "C":
            continue
        if fipsCode[0] == "C" and fipsCode[1] == "S":
            continue
        if fipsCode[0] == "M":
            continue
        if fipsCode[0] == "C" and fipsCode[1] == "M" and fipsCode[2] == "S":
            continue
        dictionary.update({line[areaFipsIndex] : line[areaTitleIndex]})
    return dictionary

def getFileObjects(userDictionary):
    ATFileObj = open(userDictionary + "/area-titles.csv")
    ASFileObj = open(userDictionary + "/2021.annual.singlefile.csv")
    return ATFileObj, ASFileObj


rpt = Report(year=2021)  	    	       

# sys.argv[0] is bigData.py
# sys.argv[1] is the directory entered

if __name__ == '__main__':
    if len(sys.argv) <= 1:
        usage("not enough arguments", 1)

    print("Reading the databases...", file=sys.stderr)  	    	       
    before = time.time()  	    	       

    ATFileObj, ASFileObj = getFileObjects(sys.argv[1])
    dicOfFipsToNames = getRelivantFipsTooNames(ATFileObj)
    ATFileObj.close()

    dicOfRowNamesToIndexes = getDicOfRowNamesToIndexes(ASFileObj)
    numAreasAL, totalAnnualWagesAL, maxAnnualWageAL, maxAnnualWageCityAL, totalEstabAL, maxEstabAL, maxEstabCityAL, totalEmplAL, maxEmplAL, maxEmplCityAL, numAreasSI, totalAnnualWagesSI, maxAnnualWageSI, maxAnnualWageCitySI, totalEstabSI, maxEstabSI, maxEstabCitySI, totalEmplSI, maxEmplSI, maxEmplCitySI = getDataFromAS(ASFileObj, dicOfRowNamesToIndexes, dicOfFipsToNames)
    ASFileObj.close()

    after = time.time()  	    	       
    print(f"Done in {after - before:.3f} seconds!", file=sys.stderr)  	    	       

    rpt.all.num_areas           = numAreasAL

    rpt.all.total_annual_wages  = totalAnnualWagesAL
    rpt.all.max_annual_wage     = [maxAnnualWageCityAL, maxAnnualWageAL]

    rpt.all.total_estab         = totalEstabAL
    rpt.all.max_estab           = [maxEstabCityAL, maxEstabAL]

    rpt.all.total_empl          = totalEmplAL
    rpt.all.max_empl            = [maxEmplCityAL, maxEmplAL]


    rpt.soft.num_areas          = numAreasSI

    rpt.soft.total_annual_wages = totalAnnualWagesSI
    rpt.soft.max_annual_wage    = [maxAnnualWageCitySI, maxAnnualWageSI]

    rpt.soft.total_estab        = totalEstabSI
    rpt.soft.max_estab          = [maxEstabCitySI, maxEstabSI]

    rpt.soft.total_empl         = totalEmplSI
    rpt.soft.max_empl           = [maxEmplCitySI, maxEmplSI]


    # Print the completed report  	    	       
    print(rpt)
