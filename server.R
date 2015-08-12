library(shiny)
library(psych)
library(car)
library(compute.es)
library(pwr)
library(beeswarm)



shinyServer(function(input, output) {
    
    options(warn=-1)
    
    bs <- reactive({
        x <- input$data1
        x <- as.numeric(unlist(strsplit(x, "[\n, \t]")))
        x <- x[!is.na(x)]
        
        y <- input$data2
        y <- as.numeric(unlist(strsplit(y, "[\n, \t]")))
        y <- y[!is.na(y)]

        result1 <- describe(x)[2:13]
        result2 <- describe(y)[2:13]
        row.names(result1) <- "Data 1  "
        row.names(result2) <- "Data 2  "
        return(list(result1, result2))
    })



    makedistPlot <- function(){
        x <- input$data1
        x <- as.numeric(unlist(strsplit(x, "[\n, \t]")))
        x <- x[!is.na(x)]
        
        y <- input$data2
        y <- as.numeric(unlist(strsplit(y, "[\n, \t]")))
        y <- y[!is.na(y)]
        
        simple.bincount <- function(x, breaks) {
            nx <- length(x)
            nbreaks <- length(breaks)
            counts <- integer(nbreaks - 1)
            for (i in 1:nx) {
                lo <- 1
                hi <- nbreaks
                if (breaks[lo] <= x[i] && x[i] <= breaks[hi]) {
                    while (hi - lo >= 2) {
                        new <- (hi + lo) %/% 2
                        if(x[i] > breaks[new])
                        lo <- new
                        else
                        hi <- new
                    }
                    counts[lo] <- counts[lo] + 1
                }
            }
            return(counts)
        }
        
        nclass.x <- nclass.FD(x)
        breaks.x <- pretty(x, nclass.x)
        counts.x <- simple.bincount(x, breaks.x)
        counts.max.x <- max(counts.x)
        
        nclass.y <- nclass.FD(y)
        breaks.y <- pretty(y, nclass.y)
        counts.y <- simple.bincount(y, breaks.y)
        counts.max.y <- max(counts.y)
        
        counts.max <- max(c(counts.max.x, counts.max.y))
        
        
        xy.min <- min(c(x,y))
        xy.min <- xy.min - xy.min*0.1
        xy.max <- max(c(x,y))
        xy.max <- xy.max + xy.max*0.1
        
        p1 <- hist(x, xlim = c(xy.min, xy.max), ylim = c(0, counts.max*1.3))
        p2 <- hist(y, xlim = c(xy.min, xy.max), ylim = c(0, counts.max*1.3))
        
        plot(p1, las=1, xlab = "Data 1 is expressed in blue; Data 2 in red. Vertical lines show the mean.",
        main = "", col = rgb(0,0,1,1/4), xlim = c(xy.min,xy.max), ylim = c(0, counts.max*1.3))
        plot(p2, las=1, xlab = "", main = "", col = rgb(1,0,0,1/4), xlim = c(xy.min,xy.max), ylim = c(0, counts.max*1.3), add = T)
        
        abline(v = mean(x), col = "blue", lwd = 2)
        abline(v = mean(y), col = "red", lwd = 2)
    }

    output$distPlot <- renderPlot({
        print(makedistPlot())
    })
    
    
    
    makeboxPlot <- function(){
        x <- input$data1
        x <- as.numeric(unlist(strsplit(x, "[\n, \t]")))
        x <- x[!is.na(x)]
        
        y <- input$data2
        y <- as.numeric(unlist(strsplit(y, "[\n, \t]")))
        y <- y[!is.na(y)]
        
        score <- c(x, y)
        group <- factor(c(rep("Data 1", length(x)), rep("Data 2", length(y))))
        
        boxplot(score ~ group, las=1, xlab= "Means and +/-1 SDs are displayed in red.")
        
        beeswarm(score ~ group, col = 4, pch = 16, add = TRUE)
        
        points(1.2, mean(x), pch = 18, col = "red", cex = 2)
        arrows(1.2, mean(x), 1.2, mean(x) + sd(x), length = 0.1, angle = 45, col = "red")
        arrows(1.2, mean(x), 1.2, mean(x) - sd(x), length = 0.1, angle = 45, col = "red")
        
        points(2.2, mean(y), pch = 18, col = "red", cex = 2)
        arrows(2.2, mean(y), 2.2, mean(y) + sd(y), length = 0.1, angle = 45, col = "red")
        arrows(2.2, mean(y), 2.2, mean(y) - sd(y), length = 0.1, angle = 45, col = "red")
    }

    output$boxPlot <- renderPlot({
        print(makeboxPlot())
    })
    
    
    
    testnorm <- reactive({
        x <- input$data1
        x <- as.numeric(unlist(strsplit(x, "[\n, \t]")))
        x <- x[!is.na(x)]
        
        y <- input$data2
        y <- as.numeric(unlist(strsplit(y, "[\n, \t]")))
        y <- y[!is.na(y)]
        
        data.1ks <- ks.test(scale(x), "pnorm")
        data.1sh <- shapiro.test(x)
        
        data.2ks <- ks.test(scale(y), "pnorm")
        data.2sh <- shapiro.test(y)
        
        return(list(Data.1 = data.1ks, Data.1 = data.1sh, Data.2 = data.2ks, Data.2 = data.2sh))
    })

    levene <- reactive({
        x <- input$data1
        x <- as.numeric(unlist(strsplit(x, "[\n, \t]")))
        x <- x[!is.na(x)]
        
        y <- input$data2
        y <- as.numeric(unlist(strsplit(y, "[\n, \t]")))
        y <- y[!is.na(y)]
        
        score <- c(x, y)
        group <- factor(c(rep("Data 1", length(x)), rep("Data 2", length(y))))

        leveneTest(score, group, center=mean)
    })
    
    t <- reactive({
        x <- input$data1
        x <- as.numeric(unlist(strsplit(x, "[\n, \t]")))
        x <- x[!is.na(x)]
        
        y <- input$data2
        y <- as.numeric(unlist(strsplit(y, "[\n, \t]")))
        y <- y[!is.na(y)]
        
        score <- c(x, y)
        group <- factor(c(rep("Data 1", length(x)), rep("Data 2", length(y))))
        
        normal.t <- t.test(score ~ group, var.equal=TRUE)
        Welch.t <- t.test(score ~ group, var.equal=FALSE)
        
        return(list(normal.t, Welch.t))
    })

    es <- reactive({
        x <- input$data1
        x <- as.numeric(unlist(strsplit(x, "[\n, \t]")))
        x <- x[!is.na(x)]
        
        y <- input$data2
        y <- as.numeric(unlist(strsplit(y, "[\n, \t]")))
        y <- y[!is.na(y)]
        
        m1 <- mean(x)
        sd1 <- sd(x)
        n1 <- length(x)
        
        m2 <- mean(y)
        sd2 <- sd(y)
        n2 <- length(y)
        
        mes(m1, m2, sd1, sd2, n1, n2)
    })

    mw <- reactive({
        U.test <- function(	x, y, correct = TRUE)
		{
            x <- x[!is.na(x)]
            y <- y[!is.na(y)]
            n1 <- length(x)
            n2 <- length(y)
            n <- n1+n2
            xy <- c(x, y)
            r <- rank(xy)
            U1 <- n1*n2+n1*(n1+1)/2-sum(r[1:n1])
            tie <- table(r)
            U <- min(U1, n1*n2-U1) # U
            V <- n1*n2*(n^3-n-sum(tie^3-tie))/12/(n^2-n) # variance ties considered
            E <- n1*n2/2 # Expected
            z <- (abs(U-E)-ifelse(correct, 0.5, 0))/sqrt(V)  # z-value
            EffectSize.r <- z/sqrt(n)
            P <- pnorm(z, lower.tail=FALSE)*2
            cat(" U =", U, ",", "E(U) =", E, ",", "V(U) =", V, "\n",
               "z-value =", z, "\n",
               "p.value =", P, "\n", "\n",
               "effect eize r =", EffectSize.r)
        }
        
        x <- input$data1
        x <- as.numeric(unlist(strsplit(x, "[\n, \t]")))
        x <- x[!is.na(x)]
        
        y <- input$data2
        y <- as.numeric(unlist(strsplit(y, "[\n, \t]")))
        y <- y[!is.na(y)]

        ut <- U.test(x, y, correct = FALSE)

    })
    
    power <- reactive({
        x <- input$data1
        x <- as.numeric(unlist(strsplit(x, "[\n, \t]")))
        x <- x[!is.na(x)]
        
        y <- input$data2
        y <- as.numeric(unlist(strsplit(y, "[\n, \t]")))
        y <- y[!is.na(y)]
        
        m1 <- mean(x)
        sd1 <- sd(x)
        n1 <- length(x)
        
        m2 <- mean(y)
        sd2 <- sd(y)
        n2 <- length(y)
        
        s.within <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2)/(n1 + n2 - 2))
        d <- (m1 - m2)/s.within
        
        posthoc <- pwr.t2n.test(n1 = n1, n2 = n2, d = d, sig.level = 0.05)$power
        
        future <- ceiling(power.t.test(power = 0.8, delta = d, sig.level = 0.05,
                          type = 'two.sample', strict = T, alternative = "two.sided")$n)
                          
        cat(" Post hoc (observed) power =", round(posthoc, 3), "\n",
            "\n",
            "  Note: According to Cumming (2012), post hoc power is 'illegitimate'", "\n",
            "        and we should NEVER calculate or report it.", "\n",
            "\n",
            "\n",
            "Sample size needed for future experiment:", "\n",
            " n =", future, "(n is number in *each* group.)", "\n",
            " Power = 0.8, sig.level = 0.05, alternative = two.sided, d =", round(d, 2), "\n",
            "\n",
            "  Note: This is true only PROVIDED that the population effect size is", "\n",
            "        equal to the observed sample effect size (i.e., it is unrealistic).", "\n",
            "\n",
            "\n",
            "POWER ANALYSIS SHOULD BE CONDUCTED PRIOR TO THE EXPERIMENT!", "\n")
    })
    
    
    info <- reactive({
        info1 <- paste("This analysis was conducted with ", strsplit(R.version$version.string, " \\(")[[1]][1], ".", sep = "")# バージョン情報
        info2 <- paste("It was executed on ", date(), ".", sep = "")# 実行日時
        cat(sprintf(info1), "\n")
        cat(sprintf(info2), "\n")
    })
    
    output$info.out <- renderPrint({
        info()
    })




    output$textarea.out <- renderPrint({
        bs()
    })
    
    output$testnorm.out <- renderPrint({
        testnorm()
    })

    output$levene.out <- renderPrint({
        levene()
    })
    
    output$t.out <- renderPrint({
        t()
    })
    
    output$es.out <- renderPrint({
        es()
    })
    
    output$mw.out <- renderPrint({
        mw()
    })
    
    output$power.out <- renderPrint({
        power()
    })
    


})