# Software Development Plan

## Phase 0: Requirements Specification *(10%)*

I am to make a recursive web crawler. 

What does that mean?

Well I need to make a program that when given an url can find all the links on the page and them click on them programmatically. 
Then look at the next page and the next and the next visiting each link only once.

I am to use the provided third party and python built in libraries. These 4 libraries should make it easy to complete the task.

The way in witch the links are all found on the page needs to be done via recursion and not with loops.


## Phase 1: System Analysis *(10%)*

Data that is used by the program:

The program needs to take in an absolute url. How that will be taken is via the sys.argv list from the terminal.

From the absolute url the HTML of the page can be taken and then program can use a recursive algorithm to find all the links on the page.

The user should be able to override how many links deep the programs searches. By default, the program will go 3 links deep.

The program will print what url it will start with and at what time. Then it will print out the links found with 4 spaces of indentation with every link found.

When the program runs out of links to visit or when the max number of links that should be found is reached then the program will end with the amount of time it took it find the links and how many where found.

## Phase 2: Design *(30%)*

* if main:
  * check if no url is passed in to the program via the argv list.
  * if there isn't then print a useful message and exit.
  * else make the url equal to whatever string is in the second position in the list.
  * then check if the url is an absolute url with the urlparse package.
  * if not then print a message and exit.
  * if there is a 3rd option in the argv list then check if it is numeric and make it the maxDepth that the program should go.
  * print what the program will do.
  * take note of what time it is.
  * pass in maxDepth and the url to the crawl method.
  * after the program is done print how long it took to run and how many unique urls where found.
* def crawl(url):
  * check the current depth of recursion by looking at the depth value. If it is equal or greater than the maxDepth then return.
  * print the url found and indentation the print as appropriate.
  * handle exceptions raised by the BeautifulSoup package in order to recover from errors thrown.
  * make a GET request with the requests' library.
  * use beautiful soup to find all the 'a' tages with a hfref.
  * make an absolute address from a possibly relative url.
  * only HTTP or HTTPS links are be followed.
  * trim fragments of the url.
  * use a set to keep track of the urls already found and visited.
  * call crawl on unvisited urls.
  
## Phase 3: Implementation *(15%)*

Coding went well. I really don't have any questions or comments other than I think it was fun building it.

## Phase 4: Testing & Debugging *(30%)*

Everything ran well other than I need to change the formatting of the amount of time it took to only have 2 decimals. 
Also, I needed to change an if statement when the url was being verified. 
Also added a line of code to get rid of any fragments that the user might have put in with the starting url.

Here are my test cases:

### Valid

#### Test 1
```
(venv) nathanstott@Nathans-MacBook-Air cs1440-Stott-Nate-assn6 % python3 src/crawler.py https://cs.usu.edu
Crawling from https://cs.usu.edu to a maximum depth of 3 links
    https://cs.usu.edu
        https://www.usu.edu/privacy/
            https://www.usu.edu/apply/
            https://www.usu.edu/visit/
            https://www.usu.edu/advancement/
            https://www.usu.edu/calendar/
            https://www.usu.edu/azindex/
            https://directory.usu.edu/
            https://library.usu.edu/
            https://my.usu.edu/
            https://www.usu.edu
            https://www.usu.edu/
            https://www.usu.edu/about/
            https://www.usu.edu/about/fast-facts/
            https://www.usu.edu/dei/
            https://www.usu.edu/about/leadership/
            https://www.usu.edu/about/history/
            https://www.usu.edu/about/alumni/
            https://www.usu.edu/today/
            https://www.usu.edu/magazine/
            https://www.usu.edu/pressroom/
            https://www.usu.edu/experts
            https://statewide.usu.edu/locations
            https://eastern.usu.edu/
            https://www.usu.edu/map/
            https://www.usu.edu/location/
            https://www.usu.edu/contact/
            https://www.usu.edu/directory/
            https://jobs.usu.edu/
            https://www.usu.edu/admissions/
            https://gradschool.usu.edu/
            https://study.usu.edu/
            https://concurrent.usu.edu/
            https://www.usu.edu/admissions/costs-and-aid/
            https://www.usu.edu/financialaid/
            https://www.usu.edu/scholarships/
            https://gradschool.usu.edu/tuition-financial-aid/
            https://www.usu.edu/calendar/academic/
            https://catalog.usu.edu/
            https://catalog.usu.edu/content.php?catoid=12&navoid=3320
            https://www.usu.edu/registrar/registration/payment/
            https://www.usu.edu/registrar/registration/
            https://www.usu.edu/registrar/records/access/
            https://www.usu.edu/admissions/whatsnext/
            https://www.usu.edu/admissions/enrollment-deposit
            https://www.usu.edu/admissions/deferment/
            https://www.usu.edu/parents/
            https://www.usu.edu/orientation/
            https://www.usu.edu/academics/
            https://www.usu.edu/academics/colleges/
            https://statewide.usu.edu/
            https://www.usu.edu/online/
            https://apply.studyabroad.usu.edu/
            https://www.usu.edu/honors/
            https://www.usu.edu/degrees/
            https://www.usu.edu/degrees/index.cfm?browse&l=undergraduate
            https://www.usu.edu/degrees/index.cfm?browse&l=graduate
            https://www.usu.edu/degrees/index.cfm?campus=29
            https://www.usu.edu/degrees/index.cfm?browse&tech
            https://www.usu.edu//advising/
            https://www.usu.edu/academic-support/
            https://www.usu.edu/veterans/
            https://www.usu.edu/ais/
            https://www.usu.edu/drc/
            https://www.usu.edu/career-design-center/
            https://catalog.usu.edu
            https://www.usu.edu/commencement
            https://www.usu.edu/campus-life/
            https://www.usu.edu/campus-life/living/
            https://www.usu.edu/campus-life/health-and-wellness/
            https://www.usu.edu/campus-life/safety/
            https://www.usu.edu/campus-life/involvement/
            https://utahstateaggies.com/
            https://www.usu.edu/campus-life/recreation/
            https://www.usu.edu/arts-culture/
            https://www.usu.edu/involvement/clubs/
            https://www.usu.edu/student-affairs/
            https://www.usu.edu/aggiewellness/
            https://www.usu.edu/inclusion/
            https://www.usu.edu/student-affairs/involvement
            https://www.usu.edu/community-engagement/
            https://www.usu.edu/student-affairs/wellness
            https://www.usu.edu/research-innovation/
            https://extension.usu.edu
            https://www.usu.edu/prospective-students/
            https://www.usu.edu/current-students/
            https://www.usu.edu/faculty-staff/
            https://www.usu.edu/alumni/
            https://www.usu.edu/questions/
            https://www.usu.edu/myusu/
            https://policies.google.com/privacy?hl=en&gl=us
            https://tools.google.com/dlpage/gaoptout
            https://www.usu.edu/copyright/
            https://www.usu.edu/licensing/
            https://www.usu.edu/copyrightatusu/
            https://www.usu.edu/contact/suggestion/
            https://www.usu.edu/request-information/
            http://www.facebook.com/UtahState
            http://twitter.com/USUAggies
            http://www.youtube.com/user/UtahStateUniversity
            http://instagram.com/usuaggielife/
            https://www.linkedin.com/school/utah-state-university/
            https://www.usu.edu/about/at-a-glance/
            https://jobs.usu.edu
            https://rgs.usu.edu/graduateschool/
            https://statewide.usu.edu
            https://canvas.usu.edu
            https://it.usu.edu/service-desk/
            https://www.usu.edu/maps/
            https://www.usu.edu/search/
            https://www.usu.edu/copyright/index.cfm
            https://accessibility.usu.edu
            https://www.usu.edu/equity/non-discrimination
        https://my.usu.edu
        https://cs.usu.edu/cs/index.php
            https://cs.usu.edu/science/index.php
            https://cs.usu.edu/cs/about/history.php
            https://cs.usu.edu/science/news/index.php
            https://cs.usu.edu/cs/about/faq.php
            https://cs.usu.edu/cs/students/new/admission.php
            https://www.usu.edu/admissions/apply/transfer
            https://www.usu.edu/registrar/transfer/credit-search
            https://www.usu.edu/orientation/guides/index
            https://cs.usu.edu/orientation/
            https://www.usu.edu/mathprep/aleks-ppl
            https://cs.usu.edu/parents/
            https://cs.usu.edu/cs/students/undergraduate/index.php
            https://cs.usu.edu/cs/students/undergraduate/pre-professional-program.php
            https://catalog.usu.edu/preview_program.php?catoid=12&poid=9373
            https://cs.usu.edu/cs/students/undergraduate/5-year-bs-ms.php
            https://cs.usu.edu/cs/students/undergraduate/minors.php
            https://cs.usu.edu/cs/students/new/challenge-exams.php
            https://cs.usu.edu/cs/students/online-certificates.php
            https://cs.usu.edu/cs/students/undergraduate/apply-for-graduation.php
            https://cs.usu.edu/cs/students/graduate/index.php
            https://cs.usu.edu/cs/students/graduate/graduate-course-authorization.php
            https://cs.usu.edu/cs/students/graduate/ms-plan-a.php
            https://cs.usu.edu/cs/students/graduate/ms-plan-b.php
            https://cs.usu.edu/cs/students/graduate/mcs.php
            https://cs.usu.edu/cs/students/graduate/ms-data-science.php
            https://cs.usu.edu/cs/students/graduate/phd.php
            https://cs.usu.edu/cs/students/resources/policies/coaching-center.php
            https://www.usu.edu/math/amlc/tutoring/index
            https://cs.usu.edu/cs/students/resources/course-descriptions.php
            https://cs.usu.edu/cs/students/resources/software.php
            https://cs.usu.edu/cs/students/resources/careers.php
            https://www.usu.edu/science/students/clubs-and-organizations
            https://cs.usu.edu/cs/students/resources/policies/index.php
            https://cs.usu.edu/cs/students/resources/policies/code-of-conduct.php
            https://cs.usu.edu/cs/students/resources/travel-award.php
            https://cs.usu.edu/science/students/scholarship/index.php
            https://cs.usu.edu/cs/advising/index.php
            https://cs.usu.edu/cs/research/funding.php
            https://cs.usu.edu/cs/research/areas-new.php
            https://cs.usu.edu/cs/research/interests-computer-science.php
            https://cs.usu.edu/cs/directory/faculty/index.php
            https://cs.usu.edu/cs/directory/staff/index.php
            https://cs.usu.edu/cs/assessment/undergraduate/index.php
            https://cs.usu.edu/cs/assessment/graduate/index.php
            https://cs.usu.edu/cs/jobs/index.php
            https://cs.usu.edu/cs/about/contact.php
            https://www.usu.edu/giving/
            https://www.usu.edu/today/story/?story=fighting-for-performance-usu-computer-scientist-tackles-exascale-challenges
            https://www.usu.edu/science/news/Mary-Veronica-Kolesar.php
            https://careers-usu.icims.com/jobs/5542/assistant-associate-professor/job
            https://www.abet.org/
            https://accessibility.usu.edu/
            https://www.usu.edu/equity/non-discrimination/
            https://a.cms.omniupdate.com/11/?skin=usu&account=usu&site=USU&action=de&path=/cs/index.pcf
Program took 24.51 seconds to run. Number of unique urls visited: 169
```

#### Test 2
```
nathanstott@Nathans-Air cs1440-Stott-Nate-assn6 % python3 src/crawler.py http://localhost:8000/
Crawling from http://localhost:8000/ to a maximum depth of 3 links
    http://localhost:8000/
        http://localhost:8000/deadend
        http://localhost:8000/a
            http://localhost:8000/aa
            http://localhost:8000/ab
            http://localhost:8000/ac
        http://localhost:8000/b
            http://localhost:8000/ba
            http://localhost:8000/bb
            http://localhost:8000/bc
        http://localhost:8000/c
            http://localhost:8000/ca
            http://localhost:8000/cb
            http://localhost:8000/cc
Program took 0.11 seconds to run. Number of unique urls visited: 14
```

#### Test 3
```
nathanstott@Nathans-Air cs1440-Stott-Nate-assn6 % python3 src/crawler.py http://localhost:8000/ 5
Crawling from http://localhost:8000/ to a maximum depth of 5 links
    http://localhost:8000/
        http://localhost:8000/deadend
        http://localhost:8000/a
            http://localhost:8000/aa
                http://localhost:8000/aaa
                    http://localhost:8000/aaaa
                    http://localhost:8000/aaab
                    http://localhost:8000/aaac
                http://localhost:8000/aab
                    http://localhost:8000/aaba
                    http://localhost:8000/aabb
                    http://localhost:8000/aabc
                http://localhost:8000/aac
                    http://localhost:8000/aaca
                    http://localhost:8000/aacb
                    http://localhost:8000/aacc
            http://localhost:8000/ab
                http://localhost:8000/aba
                    http://localhost:8000/abaa
                    http://localhost:8000/abab
                    http://localhost:8000/abac
                http://localhost:8000/abb
                    http://localhost:8000/abba
                    http://localhost:8000/abbb
                    http://localhost:8000/abbc
                http://localhost:8000/abc
                    http://localhost:8000/abca
                    http://localhost:8000/abcb
                    http://localhost:8000/abcc
            http://localhost:8000/ac
                http://localhost:8000/aca
                    http://localhost:8000/acaa
                    http://localhost:8000/acab
                    http://localhost:8000/acac
                http://localhost:8000/acb
                    http://localhost:8000/acba
                    http://localhost:8000/acbb
                    http://localhost:8000/acbc
                http://localhost:8000/acc
                    http://localhost:8000/acca
                    http://localhost:8000/accb
                    http://localhost:8000/accc
        http://localhost:8000/b
            http://localhost:8000/ba
                http://localhost:8000/baa
                    http://localhost:8000/baaa
                    http://localhost:8000/baab
                    http://localhost:8000/baac
                http://localhost:8000/bab
                    http://localhost:8000/baba
                    http://localhost:8000/babb
                    http://localhost:8000/babc
                http://localhost:8000/bac
                    http://localhost:8000/baca
                    http://localhost:8000/bacb
                    http://localhost:8000/bacc
            http://localhost:8000/bb
                http://localhost:8000/bba
                    http://localhost:8000/bbaa
                    http://localhost:8000/bbab
                    http://localhost:8000/bbac
                http://localhost:8000/bbb
                    http://localhost:8000/bbba
                    http://localhost:8000/bbbb
                    http://localhost:8000/bbbc
                http://localhost:8000/bbc
                    http://localhost:8000/bbca
                    http://localhost:8000/bbcb
                    http://localhost:8000/bbcc
            http://localhost:8000/bc
                http://localhost:8000/bca
                    http://localhost:8000/bcaa
                    http://localhost:8000/bcab
                    http://localhost:8000/bcac
                http://localhost:8000/bcb
                    http://localhost:8000/bcba
                    http://localhost:8000/bcbb
                    http://localhost:8000/bcbc
                http://localhost:8000/bcc
                    http://localhost:8000/bcca
                    http://localhost:8000/bccb
                    http://localhost:8000/bccc
        http://localhost:8000/c
            http://localhost:8000/ca
                http://localhost:8000/caa
                    http://localhost:8000/caaa
                    http://localhost:8000/caab
                    http://localhost:8000/caac
                http://localhost:8000/cab
                    http://localhost:8000/caba
                    http://localhost:8000/cabb
                    http://localhost:8000/cabc
                http://localhost:8000/cac
                    http://localhost:8000/caca
                    http://localhost:8000/cacb
                    http://localhost:8000/cacc
            http://localhost:8000/cb
                http://localhost:8000/cba
                    http://localhost:8000/cbaa
                    http://localhost:8000/cbab
                    http://localhost:8000/cbac
                http://localhost:8000/cbb
                    http://localhost:8000/cbba
                    http://localhost:8000/cbbb
                    http://localhost:8000/cbbc
                http://localhost:8000/cbc
                    http://localhost:8000/cbca
                    http://localhost:8000/cbcb
                    http://localhost:8000/cbcc
            http://localhost:8000/cc
                http://localhost:8000/cca
                    http://localhost:8000/ccaa
                    http://localhost:8000/ccab
                    http://localhost:8000/ccac
                http://localhost:8000/ccb
                    http://localhost:8000/ccba
                    http://localhost:8000/ccbb
                    http://localhost:8000/ccbc
                http://localhost:8000/ccc
                    http://localhost:8000/ccca
                    http://localhost:8000/cccb
                    http://localhost:8000/cccc
Program took 0.60 seconds to run. Number of unique urls visited: 122
```

### Invalid

#### Test 1
```
nathanstott@Nathans-Air cs1440-Stott-Nate-assn6 % python3 src/crawler.py http://localhost:8000/ daf
Program crashed because invalid second argument. See user manual for more info.
```

#### Test 2
```
nathanstott@Nathans-Air cs1440-Stott-Nate-assn6 % python3 src/crawler.py http://localhost:8000/ -1 
Program crashed because invalid second argument. See user manual for more info.
```

#### Test 3
```
nathanstott@Nathans-Air cs1440-Stott-Nate-assn6 % python3 src/crawler.py http://localhost:8000/ 1.55
Program crashed because invalid second argument. See user manual for more info.
```

#### Test 4
```
nathanstott@Nathans-Air cs1440-Stott-Nate-assn6 % python3 src/crawler.py htt://localhost:8000/  
Invalid url. See user manual for more information.
```

#### Test 5
```
nathanstott@Nathans-Air cs1440-Stott-Nate-assn6 % python3 src/crawler.py https://cs.usu.edu        
Crawling from https://cs.usu.edu to a maximum depth of 3 links
    https://cs.usu.edu
        https://www.usu.edu/privacy/
            https://www.usu.edu/apply/
            https://www.usu.edu/visit/
            https://www.usu.edu/advancement/
            https://www.usu.edu/calendar/
            https://www.usu.edu/azindex/
            https://directory.usu.edu/
            https://library.usu.edu/
            https://my.usu.edu/
            https://www.usu.edu
            https://www.usu.edu/
            https://www.usu.edu/about/
            https://www.usu.edu/about/fast-facts/
            https://www.usu.edu/dei/
            https://www.usu.edu/about/leadership/
            https://www.usu.edu/about/history/
            https://www.usu.edu/about/alumni/
            https://www.usu.edu/today/
            https://www.usu.edu/magazine/
^CProgram crashed because there was a keyboard interruption
Program took 4.60 seconds to run. Number of unique urls visited: 20
```


## Phase 5: Deployment *(5%)*

* Your repository pushed to GitLab.
  * OK
* **Verify** that your final commit was received by browsing to its project page on GitLab.
  * Ok
      * Ensure the project's URL is correct.
        * OK
      * Review the project to ensure that all required files are present and in correct locations.
        * OK
      * Check that unwanted files have not been included.
        * OM
      * Make any final touches to documentation, including the Sprint Signature and this Plan.
        * OK
* **Validate** that your submission is complete and correct by cloning it to a new location on your computer and re-running it.
  * OK
      * Run your program from the command line so you can see how it will behave when your grader runs it.  **Running it in PyCharm is not good enough!**
        * OK
      * Run through your test cases to avoid nasty surprises.
        * OK
      * Check that your documentation files are all present.
        * OK


## Phase 6: Maintenance

* Write brief and honest answers to these questions: *(Note: do this before you complete **Phase 5: Deployment**)*
  * OK
      * What parts of your program are sloppily written and hard to understand?
        * None they are all writen clearly.
            * Are there parts of your program which you aren't quite sure how/why they work?
              * No I understand it all.
            * If a bug is reported in a few months, how long would it take you to find the cause?
              * Not long the program is short.
      * Will your documentation make sense to...
          * ...anybody besides yourself?
            * Yes its a short program
          * ...yourself in six month's time?
            * Yes its a short program
      * How easy will it be to add a new feature to this program in a year?
        * very easy since the program is short
      * Will your program continue to work after upgrading...
          * ...your computer's hardware?
            * Yes its running python
          * ...the operating system?
            * Yes its running python
          * ...to the next version of Python?
            * Yes its running basic python
* Fill out the Assignment Reflection on Canvas.
  * OK
