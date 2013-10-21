library(shiny)

shinyUI(pageWithSidebar(


    headerPanel("Comparing Two Independent Samples"),


    sidebarPanel(

        p(strong("Data 1:")),
        tags$textarea(id="data1", rows=20, cols=10, "50\n56\n79\n99\n56\n66\n67\n81\n55\n44\n45\n43\n77\n72\n60\n37\n39\n56\n66\n85\n55"),

        p(br()),

        p(strong("Data 2:")),
        tags$textarea(id="data2", rows=20, cols=10, "22\n100\n45\n66\n77\n88\n76\n79\n44\n55\n65\n76\n66\n44\n32\n55\n56\n57\n77\n65\n40\n41\n49\n60"),

        p(br()),

        strong('Options:'),
        p('It will take about 20 seconds to execute these options.'),

        checkboxInput(inputId = "permutation",
            label = ("Show permutation (randomization) test"),
            value = FALSE),

        checkboxInput(inputId = "robust",
            label = ("Show robust t-test"),
            value = FALSE)

        ),


mainPanel(
    tabsetPanel(

    tabPanel("Main",

        h3("Basic statistics"),
        verbatimTextOutput("textarea.out"),

        br(),

        h3("Overlayed histograms"),
        plotOutput("distPlot"),

        h3("Box plots with individual data points"),
        plotOutput("boxPlot", width="80%"),

        br(),

        h3("Levene's test for equality of variances"),
        verbatimTextOutput("levene.out"),

        br(),

        h3("Independent t-test"),
        verbatimTextOutput("t.out"),

        br(),

        h3("Effect size indices"),
        verbatimTextOutput("es.out"),

        br(),

        h3("Permutation (Randomization) test"),
        verbatimTextOutput("perm.out"),

        br(),

        h3("Robust t-test"),
        verbatimTextOutput("robust.out"),

        br(),

        h3("Mann-Whitney U-test"),
        verbatimTextOutput("mw.out")

        ),

    tabPanel("About",

        strong('Note'),
            p('This web application is developed with',
            a("Shiny.", href="http://www.rstudio.com/shiny/", target="_blank"),
            ''),

        br(),

        strong('Input values'),
            p('Input values can be separated by newlines, spaces, commas, or tabs.'),

        br(),

        strong('Code'),
            p('Source code for this application is based on',
            a('"The handbook of Research in Foreign Language Learning and Teaching" (Takeuchi & Mizumoto, 2012).', href='http://mizumot.com/handbook/', target="_blank")),

            p('The code for this web application is available at',
            a('GitHub.', href='https://github.com/mizumot/two', target="_blank")),

            p('If you want to run this code on your computer (in a local R session), run the code below:',
            br(),
            code('library(shiny)'),br(),
            code('runGitHub("two","mizumot")')
            ),

        br(),

        strong('Recommended'),
            p('To learn more about R, I suggest this excellent and free e-book (pdf),',
            a("A Guide to Doing Statistics in Second Language Research Using R,", href="http://cw.routledge.com/textbooks/9780805861853/guide-to-R.asp", target="_blank"),
            'written by Dr. Jenifer Larson-Hall.'),

            p('Also, if you are a cool Mac user and want to use R with GUI,',
            a("MacR", href="http://www.urano-ken.com/blog/2013/02/25/installing-and-using-macr/", target="_blank"),
            'is defenitely the way to go!'),

        br(),

        strong('Author'),
            p(a("Atsushi MIZUMOTO,", href="http://mizumot.com", target="_blank"),' Ph.D.',br(),
            'Associate Professor of Applied Linguistics',br(),
            'Faculty of Foreign Language Studies /',br(),
            'Graduate School of Foreign Language Education and Research,',br(),
            'Kansai University, Osaka, Japan'),

            br(),

        a(img(src="http://i.creativecommons.org/p/mark/1.0/80x15.png"), target="_blank", href="http://creativecommons.org/publicdomain/mark/1.0/")
    )
    )
)
))