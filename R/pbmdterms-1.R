# https://raw.githubusercontent.com/Angel001/Pubmed-terms/master/pbmdterms.R

library(dplyr)
library(ggplot2)
library(rentrez)
library(RColorBrewer)

df <- data.frame(Year = integer(), N = integer(), Type = character())
field <- "[title/abstract]"
query <- c("Lasik", "PRK", "Intralase", "ReLEx", "Intracor", "Wavefront guided",
           "Phakic intraocular lens", "Multifocal IOL", "Refractive surgery")

for (i in query) {
        for (j in 2000:2014) {
                x1 <- entrez_search(db = "pubmed", term = paste0(i, field),
                                    rettype = "count", datetype="pdat",
                                    mindate = as.character(j),
                                    maxdate = as.character(j)
                )
                df <- rbind(df, data.frame(Year = j, N = x1$count, Type = i))
                Sys.sleep(0.4)
        }
}

ggplot(df, aes(x = Year, y = N, color = Type)) +
        geom_line(size = 1) +
        geom_point(size = 3) +
        xlab("") + ylab("Number of articles") +
        ggtitle("Number of articles with a given term in the title or abstract per year") +
        scale_color_brewer(palette = "Set1") +
        guides(color = guide_legend(title = NULL, nrow = 3, keywidth = 2)) +
        scale_x_continuous(breaks = seq(2000, 2014, 2)) +
        scale_y_continuous(breaks = c(seq(0, 100, 10), seq(120, 300, 20))) +
        theme(legend.position = "top",
              legend.text = element_text(size = 13),
              plot.title = element_text(face = "bold"))


df1 <- filter(df, Type != "Lasik", Type != "PRK", Type != "Refractive surgery")
ggplot(df1, aes(x = Year, y = N, color = Type)) +
        geom_line(size = 1) +
        geom_point(size = 3) +
        xlab("") + ylab("Number of articles") +
        ggtitle("Number of articles with a given term in the title or abstract per year") +
        scale_color_brewer(palette = "Set1") +
        guides(color = guide_legend(title = NULL, nrow = 2, keywidth = 2)) +
        scale_x_continuous(breaks = seq(2000, 2014, 2)) +
        scale_y_continuous(breaks = c(seq(0, 10, 2), seq(10, 50, 4))) +
        theme(legend.position = "top",
              legend.text = element_text(size = 13),
              plot.title = element_text(face = "bold"))

ggplot(df, aes(x = Year, y = N)) +
        geom_line(size = 1, color = "deepskyblue4") +
        geom_point(size =3, color = "deepskyblue4") +
        xlab("") + ylab("Number of articles") +
        ggtitle("Number of articles with a given term in the title or abstract per year") +
        facet_wrap( ~ Type, scales = "free") +
        scale_x_continuous(breaks = seq(2000, 2014, 2)) +
        theme(axis.text.x = element_text(angle=45,  hjust=1),
              strip.text = element_text(face = "bold"),
              strip.background = element_rect(fill="slategray1"),
              plot.title = element_text(face = "bold", vjust = 2))
