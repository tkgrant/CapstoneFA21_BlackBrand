#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
library(leaflet.minicharts)
library(tidyverse)
library(sf)
library(ggthemes)
library(RColorBrewer)
library(sjmisc)
library(shinythemes)
library(DT)
library(data.table)
library(rsconnect)
library(shinycssloaders)
library(readxl)
library(readr)
library(gridExtra)
library(stringr)
library(shinyjs)
library(plotly)
library(ggrepel)
library(shinydashboard)
library(mapdata)
library(plotrix)
library(scatterpie)
library(leafpop)

prettyblue <- "#232D4B"
navBarBlue <- '#427EDC'
options(
  spinner.color = prettyblue,
  spinner.color.background = '#ffffff',
  spinner.size = 3,
  spinner.type = 7
)

colors <-
  c(
    "#232d4b",
    "#2c4f6b",
    "#0e879c",
    "#60999a",
    "#d1e0bf",
    "#d9e12b",
    "#e6ce3a",
    "#e6a01d",
    "#e57200",
    "#fdfdfd"
  )


# user -------------------------------------------------------------
ui <- navbarPage(
  title = "Hampton Roads",
  selected = "overview",
  theme = shinytheme("lumen"),
  tags$head(tags$style('.selectize-dropdown {z-index: 10000}')),
  useShinyjs(),
  
  
  
  
  
  
  
  
  
  
  
  
  #Project Overview--------------------------------------------------
  navbarMenu(
    title = "Overview",
    tabPanel(
      "Project Introduction",
      value = "overview",
      fluidRow(
        style = "margin: 2px;",
        align = "center",
        # br("", style = "padding-top:2px;"),
        # img(src = "VTDSPG Logo.png", class = "topimage", width = "20%", style = "display: block; margin-left: auto; margin-right: auto;"),
        br(""),
        h1(
          strong(
            "Tracking indicators of the economic and social mobility of the Black community in Hampton Roads"
          ),
          br(""),
          h4("Data Science for the Public Good Program"),
          h4("Virginia Tech"),
          br()
        )
      ),
      fluidRow(
        style = "margin: 6px;",
        column(
          4,
          h2(strong("The Setting")),
          p(
            a(href = "https://en.wikipedia.org/wiki/Hampton_Roads", "Hampton Roads", target = "_blank"),
            "area consists of ten cities and six counties in the Southeastern region of Virginia. It is ranked as the 33rd largest MSA in the United States, the 8th largest metro area in the Southeast region, and the 2nd largest between Atlanta and Washington, DC."
          ),
          
          p(
            "The jurisdictions of Hampton Roads are the cities of Chesapeake, Franklin, Hampton, Newport News, Norfolk, Poquoson, Portsmouth, Suffolk, Virginia Beach, and Williamsburg, and the counties of Gloucester, Isle of Wright, James City, Mathews, Southampton, and York."
          ),
          p(
            "The population of the Hampton Roads MSA has been growing over the last decade with an estimated population of approximately 1.7 million in 2020, a 3% increase from 2010. This region accounts for a large percentage – about 20% - of Virginia’s state population.  "
          ),
          p(
            "Each jurisdiction in Hampton Roads has a separate municipal government, unlike some other metro areas. While there are consultations on regional issues, there are more than 20 elected independent municipal governing bodies. As such, it is imperative for our project to examine not only differences with the Virginia population but also within the localities of Hampton Roads."
          ),
          p(
            "Given its location, Hampton Roads has one of the world’s greatest natural harbors, with the ability to accommodate the largest cargo ships on the planet.  It is also known for its large military presence, shipping piers, and miles of waterfront property and beaches. Moreover, due to the diversity of the localities, Hampton Roads have a diverse set of natural resources assets ranging from 26 miles of Atlantic Ocean and Chesapeake Bay beaches, numerous state parks, wildlife refuges, Lake Drummond, and various rivers and waterways."
          )
        ),
        column(
          4,
          h2(strong("Project Background")),
          p(
            "The Black Business Research Analytics Networking and Development (Black BRAND) is a non-profit organization operating in the Hampton Roads region. One goal of Black Brand, which was established in 2016, is to improve the values of the Black families and community within the region."
          ),
          p(
            "Despite the overall economic growth in the United States, there is a significant Black-white wealth gap. In 2019, the median white household held $188,200 in wealth",
            em("– 7.8 times"),
            "that of the typical Black household ($24,100). As such, Black BRAND wants to investigate the economic well-being of the Black community in Hampton Roads.  Moreover, given the recent increase in support for black businesses, it is important to determine whether this has resulted in any economic improvement for the Black community."
          ),
          p(
            "Guided by our meetings with the Black BRAND stakeholders and Claud Anderson's PowerNomics model, there are five main pillars to measure the overall economic and social progress of the Black community in Hampton Roads."
          ),
          img(src = "Flowchart.png", style = "display:inline;margin-left: auto; margin-right: auto;", width =
                "80%"),
          p(),
          p(
            "The focus of our project is on the first two pillars: ",
            strong("Education and Economics.")
          )
        ),
        column(
          4,
          h2(strong("Project Goals")),
          p(
            "Our team aims to create a dashboard that shows the state of the Black community in the Hampton Roads region.  This will enable stakeholders to understand the myriad of past and current factors that affect the economic and social progress of Black households in Hampton. As such, data-driven recommendations can be made to improve the well-being of residents of the region."
          ),
          p(
            "We identified, acquired, and used publicly available data to provide our stakeholders, Black BRAND, with a dashboard that shows a combined view of multiple indicators for the two pillars: Education and Economics."
          ),
          p(
            "For the Education pillar, we utilized indicators across three main areas: "
          ),
          tags$ul(
            tags$li("Educators"),
            tags$li("Educational Attainment"),
            tags$li("Suspension")
          ),
          p(),
          p(
            "Under the Economics pillar, we collected indicators for four distinct issues:"
          ),
          tags$ul(
            tags$li("Income/Wealth"),
            tags$li("Labor Market"),
            tags$li("Homeownership"),
            tags$li("Health"),
            tags$li("Household Wellbeing")
          ),
          p(
            "We conducted a cross-comparison analysis with our indicators across the counties and cities in Hampton Roads.  Our project also compares the Black population against general population in Hampton Roads to determine whether racial differences exist within each locality. Moreover, we also present information for the general population in Virginia."
          ),
          p(
            "The dashboard compiles our findings and allows our stakeholders, and other interested users to explore the information dynamically."
          )
        )
      ),
      fluidRow(align = "center",
               p(tags$small(
                 em('Last updated: August 2021')
               )))
    ),
    
    tabPanel("Hampton Roads Localities",
             fluidRow(
               column(
                 3,
                 p("", style = "padding-top:30px;"),
                 h4(strong("Counties and Cities of Hampton Roads")),
                 tags$ul(
                   tags$li("City of Chesapeake"),
                   tags$li("City of Franklin"),
                   tags$li("City of Hampton"),
                   tags$li("City of Newport News"),
                   tags$li("City of Norfolk"),
                   tags$li("City of Poquoson"),
                   tags$li("City of Portsmouth"),
                   tags$li("City of Suffolk"),
                   tags$li("City of Virginia Beach"),
                   tags$li("City of Williamsburg"),
                   tags$li("Gloucester County"),
                   tags$li("Isle of Wight County"),
                   tags$li("James City County"),
                   tags$li("Mathews County"),
                   tags$li("Southampton County"),
                   tags$li("York County")
                 )
               ),
               column(7,
                      h3(
                        strong("Localities of Hampton Roads"), align = "center"
                      ),
                      withSpinner((
                        plotOutput("hampton_counties_map", width = "100%", height = "600px")
                      ))),
             )),
    tabPanel(
      "Sociodemographics",
      fluidRow(
        style = "margin: 6px;",
        h1(strong("Sociodemographic Characteristics"), align = "center"),
        p("", style = "padding-top:20px;"),
        column(
          4,
          h4(strong("Who Makes Up Hampton Roads?")),
          p(
            "We used the American Community Census data (ACS) to better understand the population in Hampton Roads and Virginia.  The ACS is a yearly survey conducted by the U.S. Census Bureau provides detailed demographic information about American household. We collected the 5-year estimates over the period 2010-2019 to compute the percent of Hampton Roads residents in each locality by race and age.  This information is also presented for the state of Virginia."
          ),
          p(
            "The black population accounts for about 30% of the total population in Hampton Roads. This is significantly greater than Virginia’s which is about 19%."
          ),
          p(
            "The age composition for the Hampton Roads region appears to be representative of Virginia's population. However, there are variations by localities. For example, there exists a large population of young adults (ages 18 to 29) in Southampton County, whereas Portsmouth has a larger population of seniors - individuals 65 years and older. This data suggests that such ages difference may play a role in the differences in economic or education indicators across localities."
          ),
        ),
        column(8,
               tabsetPanel(
                 # Tab race
                 tabPanel("Race",
                          fluidRow(
                            h1(strong("Racial Demographics"), align = "center"),
                            column(
                              6,
                              h4("Hampton Roads"),
                              
                              selectInput(
                                "hampRaceYearDrop",
                                "Select Year:",
                                width = "100%",
                                choices = c(
                                  "2019",
                                  "2018",
                                  "2017",
                                  "2016",
                                  "2015",
                                  "2014",
                                  "2013",
                                  "2012",
                                  "2011",
                                  "2010"
                                )
                              ),
                              withSpinner(plotOutput("hamp_pie")),
                              p(tags$small(
                                "Data Source: ACS 5 Year Estimate Table B02001"
                              )),
                            ),
                            
                            
                            column(
                              6,
                              
                              h4("Virginia"),
                              
                              selectInput(
                                "VaRaceYearDrop",
                                "Select Year:",
                                width = "100%",
                                choices = c(
                                  "2019",
                                  "2018",
                                  "2017",
                                  "2016",
                                  "2015",
                                  "2014",
                                  "2013",
                                  "2012",
                                  "2011",
                                  "2010"
                                )
                              ),
                              withSpinner(plotOutput("va_pie")),
                              p(tags$small(
                                "Data Source: ACS 5 Year Estimate Table B02001"
                              ))
                            )
                            
                          )),
                 #Tab age
                 tabPanel("Age",
                          fluidRow(
                            h1(strong("Age Composition of Hampton Roads"), align = "center"),
                            column(
                              6,
                              
                              h4("Hampton Roads"),
                              selectInput(
                                "HampAgeYearDrop",
                                "Select Year:",
                                width = "100%",
                                choices = c(
                                  "2019",
                                  "2018",
                                  "2017",
                                  "2016",
                                  "2015",
                                  "2014",
                                  "2013",
                                  "2012",
                                  "2011",
                                  "2010"
                                )
                              ),
                              
                              withSpinner(plotOutput("hamp_graph")),
                              p(tags$small(
                                "Data Source: ACS 5 Year Estimate Table B01001"
                              ))
                            ),
                            
                            column(
                              6,
                              
                              h4("Virginia"),
                              p(""),
                              selectInput(
                                "VaAgeYearDrop",
                                "Select Year:",
                                width = "100%",
                                choices = c(
                                  "2019",
                                  "2018",
                                  "2017",
                                  "2016",
                                  "2015",
                                  "2014",
                                  "2013",
                                  "2012",
                                  "2011",
                                  "2010"
                                )
                              ),
                              
                              withSpinner(plotOutput("va_graph")),
                              p(tags$small(
                                "Data Source: ACS 5 Year Estimate Table B01001"
                              ))
                            ),
                            
                            column(
                              12,
                              
                              h4("Hampton Roads Age Group by Localities"),
                              selectInput(
                                "HampCountAgeYearDrop",
                                "Select Year:",
                                width = "100%",
                                choices = c(
                                  "2019",
                                  "2018",
                                  "2017",
                                  "2016",
                                  "2015",
                                  "2014",
                                  "2013",
                                  "2012",
                                  "2011",
                                  "2010"
                                )
                              ),
                              withSpinner(plotOutput("age_map", height =
                                                       "600px")),
                              p(tags$small(
                                "Data Source: ACS 5 Year Estimate Table B01001"
                              ))
                            )
                            
                          ))
               ))
      )
    )
    
    
    
    
  ),
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #Data and Methodology ----------------------------------------------------
  
  
  tabPanel(
    "Data and Methodology",
    #value = "data",
    fluidRow(
      style = "margin: 6px;",
      h1(strong("Data and Methodology"), align = "center"),
      p("", style = "padding-top:10px;"),
      column(
        4,
        img(src = 'acs.png', style = "display: inline; float: left;", width = "200px"),
        p(
          "The ",
          strong("American Community Survey (ACS)"),
          " is an ongoing annual survey conducted by the U.S. Census Bureau.
                                            The ACS samples households to compile a 1-year and 5-year datasets providing information on households sociodemographic
                                            and socioeconomic characteristics. We used almost a decade (2010-2019) 5- year estimates to obtain county and city level
                                            characteristics to explore the population of Hampton Roads."
        ),
        br(),
        img(src = 'doe.jpg', style = "display: inline; float: left;", width = "150px"),
        p(
          "The ",
          strong("Virginia Department of Education (VDOE)"),
          "collects a variety of information from all school divisions in Virginia including the total number of educators and graduation rates.
                                            This allowed us to report demographic information for educators across the Hampton Roads region along with on-time graduation rate "
        ),
        br(),
        img(src = 'kidscount.png', style = "display: inline; float: left;", width = "150px"),
        p(
          strong("Kids Count"),
          "is a project of the",
          strong("Annie E. Casey Foundation"),
          "produces an annual report that assesses child well-being in the United States.
                                            We use this data source to present information on short-term suspension by race for the counties and cities in the Hampton Roads region. "
        )
      ) ,
      
      
      column(
        4,
        h5(strong("Methodology")),
        p(
          "We explore two main pillars to examine the economic and social well-being of the Black community in Hampton Roads."
        ),
        p(
          strong("Education:"),
          "Educational attainment is a major contributor to improving an individual’s economic well-being and standard of living. As such, we examine the disparities in educational attainment to determine the economic progress of the Black community.
                                          Specifically, we collected data on on-time high school graduation rates and bachelor’s degree attainment. We also include the racial/ethnic representation of educators. Research indicates some distinct, albeit small, advantages when black students are paired with a Black teacher [1]. Suspension rate was also included in our analysis as some students are more likely to be disciplined than others even for the same transgression (KIDS Count).
                                          Moreover, a higher suspension rate will limit educational attainment, given the negative effect on achievement [2].
                                            "
        ),
        img(
          src = 'education.png',
          align = "center",
          height = "300px",
          width = "400px"
        ),
        p(
          tags$small(
            "[1] Anna J. Egalitea, A.J. , Kisida B., & Winters, M.A. (2015), Economics of Education Review, 45, 44-52."
          )
        ),
        p(
          tags$small(
            "[2] Noltemeyer, A. L., Ward, R. M., & Mcloughlin, C. (2015). Relationship between school suspension and student outcomes: A meta-analysis. School Psychology Review, 44(2), 224-240."
          )
        ),
      ),
      
      
      column(
        4,
        p(),
        p(),
        p(),
        p(
          strong("Economics:"),
          "A major driver of economic growth and mobility is growth in labor, income, health, and wealth accumulation (homeownership). Thus, we examine median income, poverty rates, industry employment and unemployment rates, health insurance status, and homeownership rate for the Black and general population across the counties and cities in Hampton Roads [1]."
        ),
        p(
          "To further assess economic progress, we included several characteristics to measure Black household’s well-being.  We included welfare measures (Food Stamps or SNAP recipients); family stability (female-headed household, marital rates, grandparents’ guardianship, and migration); transportation access (private and public transportation); and technology access (computer and internet access)."
        ),
        p(),
        img(
          src = 'economic.png',
          align = "center",
          height = "300px",
          width = "400px"
        ),
        p(),
        p(),
        p(
          "Lastly, we rank counties/cities based on several of our indicators. The idea is to determine whether the Black community was consistently underperforming in some localities in the Hampton Road region."
        ),
        p(),
        p(
          tags$small(
            "[1] Butler, S. M., Beach, W. W., & Winfree, P. L. (2008). Pathways to economic mobility: Key indicators. Economic mobility project."
          )
        ),
        
      )
      
    )
    
  ),
  
  
  # Education Indicators ----------------------------------------------------
  
  
  navbarMenu(
    title = "Education",
    #Educator Demographics
    tabPanel("Educators",
             fluidRow(
               p("", style = "padding-top:20px;"),
               column(
                 4,
                 h4(strong("Who are the Educators in Hampton Roads?")),
                 p("", style = "padding-top:10px;"),
                 p(
                   "The Black population overwhelmingly attends public schools, yet in most parts of the U.S., teachers are overwhelmingly White. The National Center for Education
						Statistics reports that about 79 percent of all U.S. public school teachers were non-Hispanic White [1]. This trend is also reflected in the State of Virginia.
            For the 2020-2021 academic year, the Virginia Department of Education released racial and ethnic breakdown for educators-administrators, teachers, and other pupil personnel- by school division.
						These data enable examination of racial and ethnic representation to be matched within local school divisions.
           In 2020/21, 13 percent of public-school educators identified as Black, compared to a state rate of 19.9% Black population in 2019, a difference of almost seven percentage points [2]. "
                 ),
                 p(
                   "In 2019, 31% percent of the Hampton Roads population identified as Black.  Seven of the 16 localities (44%) reported rates of public school Black educator representation at or above the regional average, while another 7 reported rates less than 12%.
						Black educator representation ranged from high rates of 62.3% in Portsmouth and 55.8% in Franklin City to very low rates of 3.5% in Poquoson.  These percentages are reversed for White educators, with highs of 92.6% representation in Poquoson and lows of 34.6% and 40% respectively in Portsmouth and Franklin City."
                 ),
                 p(
                   "Hampton Roads localities vary considerably in their racial and ethnic profiles, making it difficult to ascertain from these data alone how well educator representation matches specific communities. A future project might be to map local demographic profiles to school division educator representation to determine
						how well Black educators are matched to local Black population representation."
                 ),
                 p("", style = "padding-top:20px;"),
                 p(
                   tags$small(
                     "[1] Taie,S. and Golding, R. (2020). Characterisitcs of Public and Private Elementary and Secondary School Teachers
						in the United States: Results from the 2017-18 National Teacher and Principal Survery. First Look. NCES 2020-142. National Center for Education Statistics"
                   )
                 ),
                 p(
                   tags$small(
                     "[2].  Virginia Department of Education, Virginia Educator Ethnicity and Race Data, https://doe.virginia.gov/teaching/workforce_data/index.shtml; U.S Census QuickFacts, https://www.census.gov/quickfacts/fact/table/VA,US/PST045219."
                   )
                 ),
               ),
               column(
                 8,
                 h1(strong("Educator by Racial and Ethnic Groups"), align = "center"),
                 p("", style = "padding-top:20px;"),
                 selectInput(
                   "teacherRaceBreakdown",
                   "Select Race:",
                   width = "100%",
                   choices = c(
                     "Black",
                     "White",
                     "American Indian",
                     "Asian",
                     "Hawaiian",
                     "Two or More Races",
                     "Hispanic"
                   )
                 ),
                 withSpinner(plotlyOutput("teacherRacePlots")),
                 p(
                   tags$small(
                     "*Note: Missing bar reflects 0 teachers for that locality's racial/ethnic group. Williamsburg and James City educator data were combined from the source dataset. "
                   )
                 ),
                 p(
                   tags$small("Source: Virginia DOE, Virginia 2020-2021 Teacher Race Report")
                 )
               )
             )),
    tabPanel("Educational Attainment",
             fluidRow(
               p("", style = "padding-top:40px;"),
               column(
                 4,
                 h4(strong("Graduating High School On-Time")),
                 p(
                   "Virginia Department of Education defines on-time graduation as the percentage of students who earned a Board of Education-approved diploma within four years of entering high school for the first time. On-time graduation rates have been steadily climbing in Virginia from 2009 thru the 2020 school year moving from 85.5% in 2010 to 92.5% in 2020. Black students have made even greater gains across this period, from 80.4% in 2011 to 91.4% graduating within four years in 2020."
                 ),
                 p(
                   "Across Hampton Roads, the same upward state trend is evident across the decade. The regional average on-time graduation rate increased steadily from 82% in 2010 to 92.6% in 2020. Black students mirrored the trend, from a regional average of 79.7% in 2010 to 92.4% in 2020, effectively matching the total regional and state population average."
                 ),
                 p(
                   "Due to the large variation between localities, we examine local differences.  For Black students, gains ranged from 7.8 to approximately 24 percentage points, with an average improvement of 17 percentage points.  Portsmouth and Southampton made the most dramatic gains, both showing upward trends from lows in 2010 of 66.1% and 72.9% respectively, to highs of 89.6% and 95% in 2020, a similar pattern to Hampton, which also showed very impressive steady gains (from 78% in 2010 to 96% in 2020)."
                 ),
                 p(
                   "While every locality showed improvement over time, two locality rates remained notably lower to the state average. Norfolk and Suffolk remained more than 6.8 and 7.2 percentage points lower than the state average in 2020. Future research could help better understand whether on-time graduation rates reflect students leaving school, obtaining alternate certification, or moving out of state without transfer notice (Suffolk in particular borders North Carolina)."
                 ),
                 
                 h4(strong("College Degree")),
                 p(
                   "A college degree is not only a 'requirement for most jobs', but leads to higher wages, financial security, and improvement in standard of living. In the U.S., there is an increasing trend of college attainment over the last decade. The ACS reports a similar trend for Virginia residents.
                                                       In 2010, 33.8% of Virginia residents aged 25 years or older had earned a bachelor's degree or higher. This rate improved to 38.8% by 2019."
                 ),
                 p(
                   "There are substantial differences in attainment rate for Black residents across the localities in Hampton Roads.  In 2019, four-year college degree rates ranged from a low of 9.8% (Poquoson) to a high of 34.8% (York).
                                                       Additionally, the differential rates between the general population and Black residents vary widely across localities by year and across years.
                                                       Some of the largest gaps are found in 2019 in Williamsburg (39.6 percentage point difference) and Poquoson (33 percentage points). The shortest gap at the end of the decade was in Hampton at 1.2 percentage points."
                 )
               ),
               column(
                 8,
                 h1(strong("Educational Attainment"), align = "center"),
                 tabsetPanel(
                   #On-time Graduation Rate
                   tabPanel("High School Graduation",
                            
                            column(12,
                                   fluidRow(
                                     p("", style = "padding-top:10px;"),
                                     h4(
                                       strong("High School On-Time Graduation Rates in Hampton Roads"),
                                       align = "center"
                                     ),
                                     withSpinner(leafletOutput("dropout_map", width = "850px")),
                                     p(tags$small(
                                       "Data Source: Virginia Department of Education"
                                     )),
                                     
                                   ))),
                   
                   tabPanel(
                     "Bachelor's Degree",
                     p(""),
                     fluidRow(
                       p("", style = "padding-top:40x;"),
                       h4(
                         strong(
                           "Percentage of Hampton Roads Population 25 years and older with Bachelor's degree or higher"
                         ),
                         align = "center"
                       ),
                       selectInput(
                         "genEdAttainmentYearDrop",
                         "Select Year:",
                         width = "100%",
                         choices = c(
                           "2019",
                           "2018",
                           "2017",
                           "2016",
                           "2015",
                           "2014",
                           "2013",
                           "2012",
                           "2011",
                           "2010"
                         )
                       ),
                       withSpinner(plotlyOutput("genEdAttainmentPlots")),
                       p(tags$small("*Note: Data missing for some years. ")),
                       p(
                         tags$small(
                           "Data Source: ACS 5 Year Estimate Tables C15002B (For Black Population), S1501 (For Total Population)"
                         )
                       )
                       
                     )
                   )
                   
                 )
               )
             )),
    
    tabPanel("Suspension",
             
             fluidRow(
               p("", style = "padding-top:60px;"),
               column(
                 4,
                 h4(strong("Who is Being Punished?")),
                 p(
                   "The Virginia Department of Education refers to students being barred from attending school for at most 10 days.  We collected short-term suspension rate by race from Kids COUNT, Annie E. Casey which is defined as the total number of offenders of a certain race divided by the total number of students of that race for each locality. Rates include students from grades K - 12."
                 ),
                 p(
                   "Across Virginia, from academic years 2014-15 to 2018-19, the Black student suspension rate was consistently more than eight percentage points higher than White and Hispanic student rates. Black students were suspended between a relative low of 12.0 % in 2018-19 and a high of 13.6 % in 2016-17. In contrast, the highest suspension rate for White and Hispanic students was 3.8 % and 3.9 % respectively, both in 2016-17."
                 ),
                 p(
                   "Black student suspension rates in Hampton Roads were decidedly higher than for White students across all years examined. This is consistent even for areas with low suspension rates - averaging rate across years, the lowest rate for Black students was 4.9% for Williamsburg-James City County which is in contrast to White students which was 2.5%. On average, the White student suspension rate was 4.8%, almost a 9-point difference."
                 ),
                 p(
                   "To illustrate gaps between Black and White student suspension rates over time, we plot the percentage point difference between Black and White rates for each Hampton Roads locality that reported more than two years of data for students of both races.  Most localities showed a Black-White student suspension gap rate of between 5 and 14 percentage points over this time period. "
                 ),
                 p(
                   "Norfolk consistently showed the greatest racial suspension gap between students, ranging from a relative low of 12.8% in 2016-17 to a high gap of 14.1% in 2018-19.  York showed the smallest gap for three of the five years, followed by Gloucester for two years."
                 )
               ),
               column(
                 8,
                 h1(strong("Short Term Suspension"), align = "center"),
                 tabsetPanel(
                   tabPanel(
                     "Suspension Rate",
                     p(""),
                     p("", style = "padding-top:10px;"),
                     h4(
                       strong("Percent of Students Suspended Short-Term in Virginia"),
                       align = "center"
                     ),
                     withSpinner(plotlyOutput("suspension_line_graph")),
                     p(tags$small(
                       "Data Source: KIDS COUNT, Annie E. Casey Foundation"
                     ))
                   ),
                   tabPanel(
                     "Suspension Rate by Race",
                     p(""),
                     p("", style = "padding-top:10px;"),
                     h4(
                       strong("Percent of Students Suspended Short-Term in Hampton Roads by Race"),
                       align = "center"
                     ),
                     selectInput(
                       "BWsuspensionYearDrop",
                       "Select Year:",
                       width = "100%",
                       choices = c("2019", "2018", "2017", "2016", "2015")
                     ),
                     withSpinner(plotlyOutput("BW_map", height = "700px")),
                     p(tags$small(
                       "Data Source: KIDS COUNT, Annie E. Casey Foundation"
                     )),
                     p(
                       tags$small(
                         "*Note: Black student data supressed for Mathews and Poquoson. Missing data for some years for Franklin for White student rates"
                       )
                     )
                   ),
                   tabPanel(
                     "Suspension Gap",
                     p(""),
                     p("", style = "padding-top:10px;"),
                     h4(
                       strong(
                         "Difference Between Black and White Student Short-Term Suspension"
                       ),
                       align = "center"
                     ),
                     withSpinner(plotlyOutput("suspensionGap", height = "700px")),
                     p(tags$small(
                       "Data Source: KIDS COUNT, Annie E. Casey Foundation"
                     ))
                   )
                   
                 )
                 
               )
               
             ))
  ),
  
  
  navbarMenu(
    title = "Economics",
    #Median Income
    tabPanel(
      "Income",
      fluidRow(
        style = "margin: 6px;",
        p("", style = "padding-top:30px;"),
        #change this later to formatting that is desired
        column(
          4,
          h4(strong(
            "Household's Economic Status in Hampton Roads"
          )),
          p(
            "In 2019, the Black community had a median annual income of $52,596, which is less than the median income of $67,387 of Hampton Road’s general population. This gap is consistent with the state trend, although it is larger at the state level, where the Black and general households earn $51,654 and $74,222, respectively."
          ),
          p(
            "While there are racial differences in median income, the gap between the Black community and the general population has been closing over the last decade. In 2010, the racial median income gap in Hampton Roads was $17,121, whereas it was $14,791 in 2019. This suggests that despite the lower median income, the Black community's economic status has slightly improved over the last decade."
          )
        ),
        h1(strong(
          "Median Income in Virginia and Hampton Roads"
        ), align = "center"),
        column(
          8,
          withSpinner(plotOutput("medianTimeGraph")),
          p(tags$small(
            "Data Source: ACS 5 Year Estimates Table S1903"
          )),
          
          selectInput(
            "MedianIncomeYearDrop",
            "Select Year:",
            width = "100%",
            choices = c(
              "2019",
              "2018",
              "2017",
              "2016",
              "2015",
              "2014",
              "2013",
              "2012",
              "2011",
              "2010"
            )
          ),
          withSpinner(plotOutput("income_plot")),
          p(tags$small(
            "Data Source: ACS 5 Year Estimates Table S1903"
          ))
        )
      )
    ),
    
    #),
    #Homeownership Map
    tabPanel("Homeownership",
             fluidRow(
               p("", style = "padding-top:20px;"),
               column(
                 4,
                 h4(strong("Home Owners")),
                 p(
                   "We report homeownership rates in Hampton Roads. The interactive map includes both the percentage of homeowners defined as total homeowners/ total population and the percentage of black homeowners calculated as the total black homeowners/total Black population.
                                         The radio buttons at the top right of the map allow the user to view the percentage of Black homeowners across the localities in Hampton Roads."
                 ),
                 p(
                   "Homeownership improves individuals' long-term wealth. It also improves civic and political engagement, children's educational outcomes, and health behaviors.
                                          As such, we examine the rate of homeownership for the Black community in Hampton Roads."
                 ),
                 p(
                   "Regardless of locality, the Black population has a lower rate of homeownership than the general population. The disparity
                                         is persistent for most counties/cities. In James City, there is a 23 percentage points difference between the general population and
                                         the Black population, with a 76% and 53.1% homeownership rate, respectively. For the other localities, differences range from 5 to 28 percentage points.
                                         Poquoson was the only locality where the Black population had a greater homeownership rate than the general population."
                 )
               ),
               column(
                 8,
                 h1(strong("Home Ownership in Hampton Roads"), align =
                      "center"),
                 withSpinner(leafletOutput("homeownership_map")),
                 p(tags$small(
                   "Data Source: ACS 5 Year Estimates Table S2505"
                 ))
               ),
             )),
    
    
    #Unemployment Rate
    tabPanel("Labor Market",
             fluidRow(
               p("", style = "padding-top:40px;"),
               column(
                 4,
                 h4(strong("Labor Sector & Unemployment")),
                 p(
                   "We examine the economic health of the labor market for the Black communities in Hampton Roads. First, we look at the top two industries that explain the highest number of workers in the region. This allows us to understand the employment landscape of the region, which may provide insights into vulnerable areas.
                                         We then analyze unemployment to understand the economic hardship that households in the Hampton Region face."
                 ),
                 p("", style = "padding-top:10px;"),
                 h5(strong (
                   "Industries that employ the largest number of workers"
                 )),
                 p(
                   "Majority of the residents in the Hampton region were employed in the Educational Services, health care, and social services sector. Among each locality in Hampton Roads, educational services, health care, and social services was one of the top two industries that employ the highest number of workers.
                                         However, for the other top employable industry, there are some slight variations across localities and time.
                                         For example, in 2012, eight of the 16 localities top two sector was in retail trade. However, only two areas had retail trade in the top two industries indicating more economic diversity among the localities over time."
                 ),
                 h5(
                   strong (
                     "Are there differences in the unemployment rate across localities?"
                   )
                 ),
                 p(
                   "The unemployment rate for the Black population in Hampton Roads tends to be greater than the average rate in Virginia, regardless of the time period. For example, in 2019, Franklin-which had the highest unemployment
                                         rate in the region- the unemployment rate for the black community was 14.9 % compared with Virginia's average of 4.6%.  Moreover, regardless of localities, the Black community is more likely to be unemployed than the other residents.  In 2019, the Black population had a higher unemployment rate than their counterparts in every county and city in the Hampton Roads region, even in areas that were below the state's average (Gloucester)."
                 ),
                 p(
                   "While for some counties there has been a decline in unemployment rates across the decade (2010-2019) for Black households, there still exists some significant disparity for most counties/cities. One such example is Williamsburg County that in 2010 had a similar unemployment rate for both Blacks and the region residents, 6.0% and 6.1%, respectively. But by 2019, that gap widen significantly by 4.6 percentage points (the Black unemployment rate was 10.4% and the total population 5.8%)"
                 ),
                 
                 
               ),
               column(
                 8,
                 h1(strong("Labor Market Characteristics"), align = "center"),
                 tabsetPanel(
                   #Sector Employment
                   tabPanel(
                     "Industry Employment",
                     p("", style = "padding-top:10px;"),
                     h4(strong("Top Two Industry Sectors"), align = "center"),
                     fluidRow(
                       p(""),
                       selectInput(
                         "SectorEmploymentYearDrop",
                         "Select Year:",
                         width = "100%",
                         choices = c(
                           "2019",
                           "2018",
                           "2017",
                           "2016",
                           "2015",
                           "2014",
                           "2013",
                           "2012",
                           "2011",
                           "2010"
                         )
                       ),
                       withSpinner(plotlyOutput("sector_plot")),
                       p(tags$small("*Note: Data missing for some years. ")),
                       p(tags$small("Source: ACS 5 Year Estimate Table DP03"))
                       
                     )
                   ),
                   
                   tabPanel(
                     "Unemployment Rate",
                     p("", style = "padding-top:10px;"),
                     h4(strong("Unemployment Rate in Hampton Roads"), align = "center"),
                     withSpinner(plotlyOutput("unemployment_plot")),
                     p(
                       tags$small(
                         "*Note: Red dotted line represents Virgina's unemployment rate. Missing data for the Black population in Mathews and Poquoson"
                       )
                     ),
                     p(tags$small(
                       "Data Source: ACS 5 Year Estimates Table S2301"
                     )),
                     sliderInput(
                       "UnemploymentRateSlider",
                       "Select Year",
                       value = 2019,
                       min = 2010,
                       max = 2019,
                       sep = "",
                       width = "100%",
                       animate =
                         animationOptions(interval = 1400)
                     ),
                   ),
                   tabPanel(
                     "Unemployment Rate Trends",
                     p("", style = "padding-top:10px;"),
                     fluidRow(
                       width = 12,
                       height = 550,
                       img(
                         src = "updated_unemployment_plot.gif",
                         height = "800",
                         width = "1100"
                       )
                       
                     )
                   )
                 )
               )
             )),
    
    
    #Poverty Rate
    tabPanel("Poverty",
             fluidRow(
               p("", style = "padding-top:50px;"),
               column(
                 4,
                 h4(
                   strong (
                     "How does the poverty rate in Hampton Roads compare to all of Virginia?"
                   )
                 ),
                 p("", style = "padding-top:10px;"),
                 p(
                   "It is clear that regardless of location the Black population has borne a higher rate of poverty than other races by about 7-8 percentage points between 2012-2019. Still, Hampton Roads has endured somewhat higher rates over the years. In 2019, 11.3% of people in Hampton Roads were in poverty, 0.7 percentage points higher than in Virginia overall. And among the Black population, the gap (1.2 percentage points) was even wider. In 2012, the gaps were smaller, but since then has gotten wider, indicating a deterioration in economic circumstances, especially for the Black population."
                 ),
                 p("", style = "padding-top:10px;"),
                 h5(
                   strong (
                     "Is poverty more prevalent in some counties or cities in Hampton Roads than others?"
                   )
                 ),
                 p(
                   "It appears that, even at the county or city level, the Black population is always more likely to be in poverty than other residents. The disparity has been pronounced and persistent for most counties/cities. Most notably, Mathews has experienced a poverty gap that has grown from about 6 percentage points in 2012 to 30 percentage points in 2019. On the other hand, some counties/cities have enjoyed a significant decline in poverty rates and gaps over time, notably Franklin City. The city had a poverty rate of roughly 36% among Blacks compared to 23% in its entire population in 2012. But by 2019, the rate decreased markedly for both groups to 22% and 15% respectively, cutting the poverty gap by half over the 8-year period."
                 ),
                 
               ),
               column(
                 8,
                 h1(strong("Poverty Rates in Hampton Roads"), align = "center"),
                 tabsetPanel(
                   tabPanel(
                     "Poverty Rates",
                     p("", style = "padding-top:10px;"),
                     h4(strong(
                       "Poverty Rates across Hampton Roads and Virginia"
                     ), align = "center"),
                     selectInput(
                       "PovertyYearDrop",
                       "Select Year:",
                       width = "100%",
                       choices = c("2019", "2018", "2017", "2016", "2015", "2014",
                                   "2013", "2012")
                     ),
                     withSpinner(plotOutput("pov_plot")),
                     p(tags$small(
                       "Data Source: ACS 5 Year Estimates Table S1701"
                     ))
                   ),
                   tabPanel(
                     "Poverty Rates across Localities",
                     p("", style = "padding-top:10px;"),
                     h4(
                       strong("Poverty Rates across Hampton Roads' Cities and Counties"),
                       align = "center"
                     ),
                     selectInput(
                       "PovertyCountYearDrop",
                       "Select Year:",
                       width = "100%",
                       choices = c("2019", "2018", "2017", "2016", "2015", "2014",
                                   "2013", "2012")
                     ),
                     withSpinner(plotlyOutput("counties_pov")),
                     p(tags$small(
                       "Data Source: ACS 5 Year Estimates Table S1701"
                     ))
                   ),
                   
                   tabPanel(
                     "Poverty Trends",
                     p("", style = "padding-top:10px;"),
                     fluidRow(
                       width = 12,
                       height = 550,
                       img(
                         src = "poverty.gif",
                         height = "800",
                         width = "1200"
                       )
                       
                     )
                   )
                   
                   
                 )
                 
               )
             )),
    
    #Uninsured Rates
    tabPanel("Health",
             fluidRow(
               p("", style = "padding-top:50px;"),
               column(
                 4,
                 h4(strong("Insurance Status In Hampton Roads")),
                 p(
                   "Health insurance is critical for preventative and emergency care and is considered a key component of financial security.  Since the Patient Protection and Affordable Care Act (ACA) passed in 2010, uninsured rates have fallen across the country. Similarly, the percentage rate for Virginias dropped from 12.3% in 2010 to 8.6% in 2019.
                                              The rates for Black Virginians were higher - in 2012, 15.4% were uninsured, whereas, in 2019, it was 10.1%. "
                 ),
                 p(
                   "The Black population of Hampton Roads showed similar trajectories to the national and state trends, with most localities showing a fairly linear falling trend.  The most dramatic change occurred in the later years, particularly in 2018 and 2019, when several localities almost or more than halved their Black uninsured rate.
                                              Most dramatically, in Franklin City, more than a quarter of the Black population (25.8%) was uninsured in 2012. By 2019, 7.3% were uninsured, a difference of 18.5 percentage points. "
                 ),
                 p(
                   "While almost all localities showed declines, many rates remained higher than the state average.
                                              Norfolk consistently remained above the state average (20.7% uninsured in 2012 to 13.8% in 2019) despite a decreasing trend. On the other hand, Chesapeake and Virginia Beach consistently had lower rates than state averages. York had by far the lowest rates over time for an area with proportional Black representation."
                 ),
                 p(
                   "Several exceptions to the general pattern also emerged. Mathews showed an increasing trend of Black residents uninsured, ranging from a relative low of 18.7% in 2013 to the highest level in 2019 at 32.4%. In stark contrast, all Black residents of Poquoson were consistently insured across the decade. Such differences may be due to the areas' small Black population. "
                 ),
                 p(
                   "Several important policy changes, as well as economic improvement following the Great Recession, may explain some of the patterns.  Full implementation of the ACA started in 2014, and in 2018 Virginia expanded Medicaid under the ACA.  Thus, there may be further improvements in future data."
                 )
                 
               ),
               column(8,
                      fluidPage(
                        h1(strong("Health Uninsured Rates"), align = "center"),
                        withSpinner(plotlyOutput("uninsured_plot")),
                        sliderInput(
                          "UninsuredPctSlider",
                          "Select Year",
                          value = 2019,
                          min = 2012,
                          max = 2019,
                          sep = "",
                          width = "100%",
                          animate = animationOptions(interval = 1400)
                        ),
                        p(
                          tags$small("*Note: Data missing for Black population in Poquoson.")
                        ),
                        p(tags$small(
                          "Data Source: ACS 5 Year Estimates Table S2701"
                        ))
                      ))
               
             )),
    
    
    #Veteran Status
    
    tabPanel("Veterans",
             fluidRow(
               p("", style = "padding-top:50px;"),
               column(
                 4,
                 h4(strong("Who has served in Hampton Roads?")),
                 p(
                   "Hampton Roads is rich in military bases and establishments. All branches of the U.S. military have bases or substantial presence, with Naval Station Norfolk being the largest naval station in the United States, if not the world [1].  Starting in the 1960s, Black Americans have been overrepresented in the United States Armed Services relative to their proportion of the population [2].
                                       Among other benefits, many view the military as an economically cost-effective means to gain training and educational advancement.   "
                 ),
                 p(
                   "We report rates of Black veteran status as a percentage of the total Black population by locality. Percentages were calculated by dividing the Black veteran population by the total Black population for each locality.
                                       The radio buttons at the top right of the map allow the user to view military bases across Hampton Roads. "
                 ),
                 p(
                   "We report rates of Black veteran status as a percentage of the total Black population by locality. Percentages were calculated by dividing the Black veteran population by the total Black population for each locality.  The radio buttons at the top right of the map allow the user to view military bases across Hampton Roads."
                 ),
                 p("", style = "padding-top:40px;"),
                 p(
                   tags$small(
                     "[1]  Hampton Roads Chamber of Commerce, 2021. Retrieved from:",
                     a(
                       href = "https://www.hrchamber.com/page/our-military/",
                       "https://www.hrchamber.com/page/our-military/",
                       target = "_blank"
                     )
                   )
                 ),
                 p(
                   tags$small(
                     "[2]. Ferguson, P. (2021).  U.S. Army. Retrieved from:",
                     a(
                       href = "https://www.army.mil/article/243604/african_american_service_and_racial_integration_in_the_u_s_military",
                       "army.mil/article/243604/african_american_service_and_racial_integration_in_the_u_s_military",
                       target = "_blank"
                     )
                   )
                 ),
                 
               ),
               column(8,
                      fluidPage(
                        h1(strong("Veteran Status in Hampton Roads"), align = "center"),
                        withSpinner(leafletOutput("veteran_map")),
                        p(tags$small(
                          "Data Source: ACS 5 Year Estimates Table S2101"
                        )),
                        sliderInput(
                          "VeteranSlider",
                          "Select Year:",
                          value = 2019,
                          min = 2010,
                          max = 2019,
                          sep = "",
                          width = "100%",
                          animate = animationOptions(interval = 1200)
                        )
                      ),)
             )),
    
    tabPanel("Household Wellbeing",
             fluidRow(
               p("", style = "padding-top:20px;"),
               column(
                 4,
                 h4(strong("Household Well-being Characteristics")),
                 p("", style = "padding-top:10px;"),
                 p(
                   "We retrived several indicators using the 2019 5-year ACS estimates to examine the well-being of Black households in Hampton Roads. These characteristics
                                               are presented using interactive maps. Clicking on the tabs and selecting a variable populates the map. Hovering over the map displays the percentage of Black residents living
                                                in a specific city/county with the selected characteristics. "
                 ),
                 p("", style = "padding-top:20px;"),
                 tags$ul(
                   tags$li(("The interactive map shows:")),
                   p("", style = "padding-top:20px;"),
                   withSpinner(textOutput("description_text"))
                 )
               ),
               column(8,
                      fluidPage(
                        h1(strong("Household Characteristics"), align = "center"),
                        selectInput(
                          "select_wellbeing",
                          "Select Indicator:",
                          width = "100%",
                          choices = c(
                            "Percent of Black Households Receiving Foodstamps/SNAP Benefits",
                            "Percent of Black Children under 18 in Female Head of Household",
                            "Percent of Married Black Population 15 years and over",
                            "Percent of Black Grandparents who are Guardians",
                            "Percent of Black County Migration",
                            "Percent of Black Population that uses car/truck/van to get to work",
                            "Percent of Black Population that uses public transportation to get to work",
                            "Percent of Black Households with a computer with broadband internet",
                            "Percent of Black Households without a computer"
                          )
                        ),
                        withSpinner(leafletOutput("wellbeing_maps")),
                        p(
                          tags$small(
                            "Data Source: ACS 5 Year Estimates Tables: S0901, S2201, S0701, S1002, S1201, S0802, S2802"
                          )
                        ),
                        
                      ))
             ))
    
  ),
  
  
  tabPanel("Future Steps",
           fluidRow(
             p("", style = "padding-top:20px;"),
             column(
               4,
               p("", style = "padding-top:10px;"),
               h4(strong("Future Steps")),
               p("", style = "padding-top:5px;"),
               p(
                 "We provided graphs that rank selected education and economic indicators for each locality in Hampton Roads. We use the 2019 ACS 5-year estimates data to create these rankings. Users can select different indicators that present a bar graph that ranks each locality according to the specific indicator.
                                     We provided this information to determine whether there were some localities in the Hampton Roads region that the Black community was consistently underperforming.  Caution should be noted for the rankings for Poquoson and Mathews County due to the small area size and Black population for both localities.
                                     This has resulted in some outliers and unavailability of data for these two areas for majority of the indicators."
               ),
               p(
                 "The rankings show that there are considerable variations across each county and city in the Hampton region. For instance, Black households seem to be less vulnerable in areas in technology access, welfare assistance, poverty, access to health insurance, and suspension rates in York County. However, they are extremely vulnerable in educational attainment and income/wealth accumulation (that is median income and homeownership).
                                     This trend is similar for all localities. In Chesapeake, there seems to be less vulnerability in areas of health insurance, transportation, technology access, unemployment, and poverty rate. On the other hand, there is a high vulnerability for income/wealth accumulation, welfare assistance, and educational attainment. Given that each jurisdiction in Hampton Roads has a separate municipality, it may be more insightful to analyze the rankings of the indicators within localities rather than making comparisons across localities to inform policymaking.  "
               ),
               p(
                 "The indicators that we have presented in our dashboard, along with the rankings, provide evidence that there are some areas that the Black community needs assistance with to improve economic and social well-being. Data-driven assistance programs in employment, obtaining health insurance, education, homeownership, and improving household stability can foster economic mobility in the Hampton Regions."
               ),
               p(
                 "For this project, we have provided a dashboard that shows a combined view of multiple indicators in two major areas: Education and Economics that collectively addresses the challenges facing the Black communities in the Hampton Region. However, given the substantial variations across localities, a future project would be to consider using the indicators provided to develop an economic and social vulnerability
                                     index specific for the Black community in the Hampton region. This future project could also include the other three pillars: Media, Art, and Entertainment, People and Values, and Politics and Justice to provide a robust and comprehensive index and dashboard for Black BRAND."
               )
               
             ),
             column(8,
                    fluidPage(
                      h1(strong("Hampton Roads Rankings"), align = "center"),
                      selectInput(
                        "select_rank",
                        "Select Indicator:",
                        width = "100%",
                        choices = c(
                          "Percentage of Black Students 25 yrs and over that have Bachelor's Degree or Higher",
                          "Percentage of Black Students Suspended",
                          "Black Median Income",
                          "Percentage of Black HomeOwners",
                          "Black Unemployment rate",
                          "Black Poverty rate",
                          "Percent of Uninsured (Health) Black population",
                          "Percentage of Black Children under 18 in Female Headed Household",
                          "Percentage of Black Population that uses car/truck/van for work",
                          "Percent of Black Households without a computer"
                        )
                      ),
                      withSpinner(plotlyOutput("ranked_chart")),
                      p(tags$small("*Note: Data missing for some localities ")),
                      p(
                        tags$small(
                          "Data Source: 2019 ACS 5 Year Estimates Tables: C15002B, KIDS Count, S1903, S2505, S2301, S1701, S2701, S0901, S0802, S2802"
                        )
                      ),
                      
                    ))
           )),
  
  navbarMenu(
    title = "Meet the Team",
    tabPanel(
      "VT DSPG",
      value = "team",
      fluidRow(column(3),
               column(
                 6,
                 h4(strong(
                   "Virginia Tech Data Science for the Public Good"
                 ) , align = "center"),
                 p(
                   "The",
                   a(
                     href = 'https://aaec.vt.edu/academics/undergraduate/beyond-classroom/dspg.html',
                     'Data Science for the Public Good (DSPG) Young Scholars program',
                     target = "_blank"
                   ),
                   "is a summer immersive program held at the",
                   a(
                     href = 'https://aaec.vt.edu/s',
                     'Virginia Tech Department of Agricultural and Applied Economics.'
                   ),
                   "In its second year, the program engages students from across the country to work together on projects that address state, federal, and local government challenges around critical social issues relevant in the world today. DSPG young scholars conduct research at the intersection of statistics, computation, and the social sciences to determine how information generated within every community can be leveraged to improve quality of life and inform public policy. For more information on program highlights, how to apply, and our annual symposium, please visit",
                   a(
                     href = 'https://aaec.vt.edu/academics/undergraduate/beyond-classroom/dspg.html',
                     'the official VT DSPG website.',
                     target = "_blank"
                   )
                 ),
                 #p("", style = "padding-top:10px;")
               )),
      fluidRow(
        column(2),
        column(
          2,
          align = "center",
          h4(strong("Graduate Fellow")),
          tags$br(),
          tags$br(),
          img(
            src = "fellow-seth.png",
            style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;",
            width = "45%",
            height = "45%"
          ),
          tags$br(),
          p(
            a(href = 'https://www.linkedin.com/in/aviseth/', 'Avi Seth', target = '_blank'),
            "(Virginia Tech, Computer Science)"
          )
        ),
        column(
          2,
          align = "center",
          h4(strong("Undergraduate Interns")),
          tags$br(),
          img(src = "BurkholderHeadshot.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
          p(
            a(href = 'https://www.linkedin.com/in/matthew-burkholder-297b9119a/', 'Matthew Burkholder', target = '_blank'),
            "(Virginia Tech, Philosophy, Politics, & Economics)"
          ),
          tags$br(),
          img(src = "Mukora.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
          tags$br(),
          p(
            a(href = "https://www.linkedin.com/in/victormukora/", 'Victor Mukora', target = '_blank'),
            "(Virginia Tech, Computational Modeling and Data Analytics)"
          ),
          tags$br(),
          img(src = "Christina_Prisbe_Headshot.jpg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
          tags$br(),
          p(
            a(href = "https://www.linkedin.com/in/christina-prisbe-60966b218/?midToken=AQGZJw6kSGscnQ&midSig=0iRgRUj5PNWpQ1&trk=eml-email_m2m_invite_single_01-hero-4-prof%7Ecta&trkEmail=eml-email_m2m_invite_single_01-hero-4-prof%7Ecta-null-ec0b8k%7Ekrp4eqqv%7E3v-null-neptune%2Fprofile%7Evanity%2Eview", 'Christina Prisbe', target = '_blank'),
            "(Virginia Tech, Computational Modeling and Data Analytics)"
          ),
          tags$br(),
          img(src = "kwabe.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
          tags$br(),
          p(
            a(href = "https://www.linkedin.com/in/kb1999/", 'Kwabe Boateng', target = '_blank'),
            "(Virginia State University, College of Engineering and Technology)"
          ),
          
        ),
        column(
          2,
          align = "center",
          h4(strong("Faculty Advisors")),
          tags$br(),
          img(
            src = "Dr_Holmes.png",
            style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;",
            height = "150px",
            width = "150px"
          ),
          tags$br(),
          p(
            a(href = 'https://aaec.vt.edu/people/faculty/holmes-chanita.html', 'Dr. Chanita Holmes', target = '_blank'),
            "(Virginia Tech, Department of Agricultural and Applied Economics)"
          ),
          tags$br(),
          img(
            src = "Dr_Bradburn.jpg",
            style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;",
            height = "150px",
            width = "150px"
          ),
          tags$br(),
          p(
            a(href = 'https://liberalarts.vt.edu/departments-and-schools/department-of-human-development-and-family-science/faculty/isabel-bradburn.html', 'Dr. Isabel Bradburn', target = '_blank'),
            "(Virginia Tech, Department of Human Development and Family Science)"
          ),
          tags$br()
        ),
        
        column(
          2,
          align = "center",
          h4(strong("Stakeholders")),
          tags$br(),
          img(src = "MalloryTuttle.jpg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
          p(
            a(href = 'https://www.linkedin.com/in/mallory-taylor-tuttle/', 'Mallory Tuttle', target = '_blank'),
            "(Associate Director Virginia Tech Hampton Roads Centers)"
          ),
          tags$br()
        )
        
      )
    ),
    tabPanel(
      "CMDA Capstone",
      value = "team",
      fluidRow(column(3),
               column(
                 6,
                 h4(
                   strong(
                     "Computational Modeling & Data Analytics Capstone Team (Fall 2021)"
                   ) ,
                   align = "center"
                 ),
                 p(
                   "The",
                   a(
                     href = 'https://www.ais.science.vt.edu/academics/cmda.html',
                     'Computational Modeling and Data Analytics (CMDA) program',
                     target = "_blank"
                   ),
                   "draws on expertise from three primary departments at Virginia Tech with strengths in quantitative science: ",
                   a(href = 'https://math.vt.edu/', ' Mathematics'),
                   ', ',
                   a(href = 'https://www.stat.vt.edu/', 'Statistics'),
                   ', ',
                   a(href = 'https://cs.vt.edu/', ' Computer Science'),
                   '.',
                   "By combining elements of these disciplines in innovative, integrated courses that emphasize techniques at the forefront of applied computation, we teach a rich suite of quantitative skills for tackling today's massive data-based problems."
                 )
                 #p("", style = "padding-top:10px;")
               )),
      fluidRow(
        column(2),
        column(
          2,
          align = "center",
          h4(strong("")),
          tags$br(),
          tags$br(),
          img(
            src = "sana.jpeg",
            style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;",
            width = "85%",
            height = "85%"
          ),
          tags$br(),
          p(
            a(href = 'https://www.linkedin.com/in/sana-abbas-963138187/', 'Sana Abbas', target = '_blank'),
            "(Virginia Tech, CMDA)"
          )
        ),
        column(
          2,
          align = "center",
          h4(strong("")),
          tags$br(),
          tags$br(),
          img(
            src = "talib.jpeg",
            style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;",
            width = "85%",
            height = "85%"
          ),
          tags$br(),
          p(
            a(href = 'https://www.linkedin.com/in/talibgrant/', 'Talib Grant', target = '_blank'),
            "(Virginia Tech, CMDA)"
          )
        ),
        column(
          2,
          align = "center",
          h4(strong("")),
          tags$br(),
          tags$br(),
          img(
            src = "caleb.jpeg",
            style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;",
            width = "85%",
            height = "85%"
          ),
          tags$br(),
          p(
            a(href = 'https://www.linkedin.com/in/caleb-slaughter-2b904a154/', 'Caleb Slaughter', target = '_blank'),
            "(Virginia Tech, CMDA)"
          )
        ),
        
        column(
          2,
          align = "center",
          h4(strong("")),
          tags$br(),
          tags$br(),
          img(
            src = "eva.jpeg",
            style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;",
            width = "85%",
            height = "85%"
          ),
          tags$br(),
          p(
            a(href = 'https://www.linkedin.com/in/eva-whaley-2022/', 'Eva Whaley', target = '_blank'),
            "(Virginia Tech, CMDA)"
          )
        )
        
      )
    )
  ),
  inverse = T
)

return(ui)
