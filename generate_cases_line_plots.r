library(zoo) 
library(ggplot2)
library(plotly)
library(htmlwidgets)
library(readr)

library(grid)

Sys.setlocale("LC_TIME", "hr_HR.UTF-8")

# num days to plot
n <- 100

# read latest data
diff_df <- read_csv('data/latest/diff_df.csv')

load('data/latest/percentage_change.Rda')
load('data/latest/cumulative_cases.Rda')
load('data/latest/avg7_df.Rda')

load('data/latest/last_date.Rda')
load('data/latest/last_date_.Rda')


plot_df <- merge(diff_df, percentage_change, by=c("Datum"))



data_to_plot <- tail(plot_df, n=n)

f <- list(size = 18, color = "black")
f2 <- list(size = 16, color = "black")

for(use_log_scale in c(FALSE, TRUE)) {
  p <- list()
  a <- list()
  
  for(i in 1:22) {
    title <- colnames(data_to_plot)[1 + i]
    
    if (i == 21) {
      title <- gsub("[.]", "", title)
    }
    else  
    {
      title <- gsub("[.]", "-", title)
    }
    
    if ((i - 1) %% 5 == 0) {
      ylab <- ylab("Broj slučajeva")
    }
    else {
      ylab <- ylab('')
    }
    
    p[[i]] <- ggplot(data=data_to_plot, aes_string(x='Datum', y=colnames(data_to_plot)[1 + i])) +
      ylab +
      geom_bar(stat="identity") +
      geom_col(aes_string(fill=colnames(data_to_plot)[1 + i + 22])) +
      scale_fill_distiller(palette = "RdYlGn", limits = c(-50, 50), oob = scales::oob_squish_any, name='') +
      geom_line(data=tail(avg7_df, n=n), aes_string(x='Datum', y=colnames(data_to_plot)[1 + i]), size=1, colour='blue') +  #
      theme_minimal() +
      theme(axis.text.x = element_text(size = 10)) +
      theme(plot.margin = unit(c(1,1,1,1), "cm"))
 
    if (use_log_scale) {
      p[[i]] <- p[[i]] + scale_y_continuous(trans=scales::pseudo_log_trans(base = 10))
    }
    
    p[[i]] <- ggplotly(p[[i]])
    
    a[[i]] <- list(
      text = paste('\n<b>', title, '</b> (', diff_df[nrow(diff_df), i], ')\nTjedna razlika <b>', sprintf("%+d", round(data_to_plot[nrow(data_to_plot), 1 + i + 22], 0)), '</b>%', sep=''),
      font = f,
      xref = "paper",
      yref = "paper",
      yanchor = "bottom",
      yshift = 10,
      xanchor = "center",
      align = "center",
      cliponaxis = FALSE,
      x = 0.5,
      y = 0.92, # 0.92
      showarrow = FALSE
    )
    
    p[[i]] <- p[[i]] %>%
      layout(annotations = a[i])
  }
  
  s <- subplot(p, nrows = 5, margin=c(0.01,0.0,0.04,0.05), titleY = TRUE, heights=c(0.17, 0.22, 0.22, 0.22, 0.17)) %>% # 2 0 5 4
    add_annotations(x = 0.65,
                    y = 0.07,
                    text = paste('<b>COVID 19 u Hrvatskoj: Pregled broja zaraženih po županijama (', last_date ,')</b>\n\nBoje prikazuju promjenu broja slučajeva zadnjih tjedan dana u usporedbi s brojem slučajeva prethodnog tjedna.\nBroj novih slučajeva u prošlih 24 sata prikazan je u zagradi kraj imena županije.\n\nGenerirano: ', format(Sys.time() + as.difftime(1, units="hours"), '%d.%m.%Y. %H:%M:%S h'), '\nIzvor podataka: koronavirus.hr\n\nAutor: Petar Palašek, ppalasek.github.io', sep=''),
                    font = f2,
                    xref = "paper",
                    yref = "paper",
                    align='left',
                    ax = 0,
                    ay = 0)
  
  if (use_log_scale) {
    saveWidget(s, file = "html/index_log.html", title = "COVID 19 u Hrvatskoj: Pregled broja zaraženih po županijama")
  }
  else {
    saveWidget(s, file = "html/index.html", title = paste("COVID 19 u Hrvatskoj: Pregled broja zaraženih po županijama (", last_date, ")", sep=""))
    
    
    orca(s, file = paste('img/', last_date_, '_line_plots.png', sep = ''), width = 27 * 72, height = 13 * 72)
  }
}
