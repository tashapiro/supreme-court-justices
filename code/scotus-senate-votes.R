library(tidyverse)
library(ggparliament)
library(rvest)
library(ggiraph)
library(ggimage)
library(htmlwidgets)
library(showtext)
library(sysfonts)


font_add_google("Chivo", "chivo")
showtext_auto()


get_votes <- function(nominee, url) {
  
  votes_html<-url%>%
    read_html()%>%
    html_elements("span.contenttext")
  
  votes<-votes_html[1]%>%
    html_text()
    votes_data<-data.frame(strsplit(votes, "\n")[[1]])
    names(votes_data)<-"col1"
  
  votes_data<-votes_data%>%
    separate(col1, sep=", ", into=c("senator","vote"))%>%
    separate(senator, sep="\\(", into=c("senator","party_state"))%>%
    separate(party_state, "-", into=c("party","state"))%>%
    mutate(state= str_replace(state,"\\)",""))
  
  votes_data$nominee <- nominee 
  
  votes_data["party"][votes_data["party"] == "ID"] <- "I"
  votes_data["party"][votes_data["senator"] == "Burdick"] <- "D"
  votes_data$party<-factor(votes_data$party, levels=c("D","R","I"))
  votes_data$vote<-factor(votes_data$vote, levels=c("Yea","Nay","Not Voting"))
  total <- votes_data%>%group_by(party)%>%summarise(seats=n())
  
  semi <- parliament_data(election_data = total,
                          type = "semicircle", # Parliament type
                          parl_rows = 5,      # Number of rows of the parliament
                          party_seats = total$seats) # Seats per party
  
  data<-cbind(votes_data%>%arrange(party,vote), semi%>%select(x,y))
  
  data
}

kagan<-get_votes("Kagan", "https://www.senate.gov/legislative/LIS/roll_call_votes/vote1112/vote_111_2_00229.htm")
barrett<-get_votes("Barrett", "https://www.senate.gov/legislative/LIS/roll_call_votes/vote1162/vote_116_2_00224.htm")
kavanaugh<-get_votes("Kavanaugh", "https://www.senate.gov/legislative/LIS/roll_call_votes/vote1152/vote_115_2_00223.htm")
ginsburg<-get_votes("Ginsburg", "https://www.senate.gov/legislative/LIS/roll_call_votes/vote1031/vote_103_1_00232.htm")
gorsuch<-get_votes("Gorsuch", "https://www.senate.gov/legislative/LIS/roll_call_votes/vote1151/vote_115_1_00111.htm")
sotomayer<-get_votes("Sotomayer", "https://www.senate.gov/legislative/LIS/roll_call_votes/vote1111/vote_111_1_00262.htm")
alito<-get_votes("Alito", "https://www.senate.gov/legislative/LIS/roll_call_votes/vote1092/vote_109_2_00002.htm")
thomas<-get_votes("Thomas","https://www.senate.gov/legislative/LIS/roll_call_votes/vote1021/vote_102_1_00220.htm")
breyer<-get_votes("Breyer","https://www.senate.gov/legislative/LIS/roll_call_votes/vote1032/vote_103_2_00242.htm")
jackson<-get_votes("Jackson","https://www.senate.gov/legislative/LIS/roll_call_votes/vote1172/vote_117_2_00134.htm")
souter<-get_votes("Souter","https://www.senate.gov/legislative/LIS/roll_call_votes/vote1012/vote_101_2_00259.htm")
roberts<-get_votes("Roberts","https://www.senate.gov/legislative/LIS/roll_call_votes/vote1091/vote_109_1_00245.htm")


nominees<-rbind(jackson, kagan,barrett,kavanaugh, ginsburg, gorsuch, sotomayer, alito,thomas,breyer,roberts, souter)

data<-nominees%>%
  mutate(
    stroke_col = case_when(party=='D'~'#3774B7',
                           party == 'R'~ '#D73A30',
                           party == 'I' ~ 'grey40')
  )%>%
  mutate(
    fill_col = case_when(vote=='Yea' ~ stroke_col,
                         TRUE ~ 'white')
  )

data$nominee<-toupper(data$nominee)

levels = c("JACKSON","BARRETT","KAVANAUGH","GORSUCH","SOTOMAYER","KAGAN","ALITO","ROBERTS","BREYER","GINSBURG","THOMAS","SOUTER")

data$nominee<-factor(data$nominee, levels=levels)

nominees<-unique(data$nominee)

profiles<-data.frame(nominees)

profiles<-data%>%distinct(nominee)
profiles$text<-as.character(profiles$nominee)
profiles$image<-paste0("https://raw.github.com/tashapiro/supreme-court-justices/main/images/",profiles$text,".png", sep="")

fake_points<-data.frame(x=c(0,0), 
                        y=c(0,0) ,
                        type=c("Yea","Nay or Not Voting"),
                        nominee = c("BARRETT","KAVANAUGH"))

fake_points$nominee<-factor(fake_points$nominee, levels=c("BARRETT","KAVANAUGH"))

presidents<-data.frame(
  nominee = c("JACKSON","BARRETT","KAVANAUGH","GORSUCH","SOTOMAYER","KAGAN","ALITO","ROBERTS","BREYER","GINSBURG","THOMAS","SOUTER"),
  presidents= c("Joe Biden","Donald Trump","Donald Trump","Donald Trump","Barack Obama","Barack Obama",
                "George W. Bush","George W. Bush","Bill Clinton","Bill Clinton","George H.W. Bush","George H.W. Bush")
)

presidents$nominee<-factor(presidents$nominee, levels=levels)

ggplot(data, aes(x=x, y=y, color=stroke_col, fill=fill_col))+
  geom_point(shape=21, size=6, stroke=1.2)+
  geom_point(data=fake_points, inherit.aes=FALSE, mapping=aes(x=x,y=y, shape=type))+
  geom_text(inherit.aes=FALSE, data=presidents, mapping=aes(label=presidents, y=2.45, x=0), size=3)+
  geom_image(data=profiles, inherit.aes=FALSE, mapping=aes(x=0,y=0.35,image=image), size=0.2, asp=1.6)+
  scale_color_identity(labels = c("Democrat", "Republican","Independent","NA"),
                                  guide = guide_legend(title="PARTY", override.aes = list(shape = 20, size=8)))+
  scale_shape_manual(labels=c("Yea", "Nay or No Vote"), values=c(20, 21), 
                     guide=guide_legend(title="VOTE", override.aes=list(color="black", size=8, stroke=1.2)))+
  scale_fill_identity()+
  facet_wrap(~nominee, ncol=3)+
  scale_y_continuous(limits=c(-0.1,2.5))+
  labs(title="SUPREME COURT CONFIRMATIONS",
       subtitle="Senate Roll Call for Supreme Court Justice Nominees",
       caption= "Data from senate.gov | Graphic by @tanya_shapiro"
       )+
  theme_void()+
  theme(text=element_text(family="chivo"),
        legend.position = c(0.5,1.08),
        legend.direction = "horizontal",
        legend.box="horizontal",
        legend.title=element_text(face="bold"),
        plot.title=element_text(face="bold",hjust=0.5, margin=margin(b=10)),
        plot.subtitle=element_text(hjust=0.5, margin=margin(b=50)),
        plot.margin=margin(t=20, b=20, r=20, l=20),
        plot.caption = element_text(margin=margin(t=10)),
        strip.text=element_text(face="bold", size=11))


#ggsave("supremes.jpeg", height=9, width=12, dpi= 600)


data$id<-1:1200
data$tooltip <- paste("Senator:  ",data$senator,'- ',data$state, 
                       "\n Vote: ", data$vote)

plot<-ggplot(data, aes(x=x, y=y, color=stroke_col, fill=fill_col))+
  geom_point_interactive(aes(tooltip=tooltip, data_id=id), shape=21, size=6, stroke=1.2)+
  geom_point(data=fake_points, inherit.aes=FALSE, mapping=aes(x=x,y=y, shape=type))+
  geom_text(inherit.aes=FALSE, data=presidents, mapping=aes(label=presidents, y=2.45, x=0), size=3.5)+
  geom_image(data=profiles, inherit.aes=FALSE, mapping=aes(x=0,y=0.35,image=image), size=0.2, asp=1.6)+
  scale_color_identity(labels = c("Democrat", "Republican","Independent","NA"),
                       guide = guide_legend(title="PARTY", override.aes = list(shape = 20, size=8)))+
  scale_shape_manual(labels=c("Yea", "Nay or No Vote"), values=c(20, 21), 
                     guide=guide_legend(title="VOTE", override.aes=list(color="black", size=8, stroke=1.2)))+
  scale_fill_identity()+
  facet_wrap(~nominee, ncol=3)+
  scale_y_continuous(limits=c(-0.1,2.5))+
  labs(title="SUPREME COURT CONFIRMATIONS",
       subtitle="Senate Roll Call for Supreme Court Justice Nominees",
       caption= "Data from senate.gov | Graphic by @tanya_shapiro"
  )+
  theme_void()+
  theme(text=element_text(family="chivo"),
        legend.position = c(0.5,1.08),
        legend.direction = "horizontal",
        legend.box="horizontal",
        legend.title=element_text(face="bold"),
        plot.title=element_text(face="bold",hjust=0.5, size=14, margin=margin(b=10)),
        plot.subtitle=element_text(hjust=0.5, margin=margin(b=80)),
        plot.margin=margin(t=20, b=20, r=20, l=20),
        plot.caption = element_text(margin=margin(t=20)),
        strip.text=element_text(face="bold", size=11))


tooltip_css <- "background-color:black;color:white;font-family:Gill Sans;padding:10px;border-radius:5px"


x<-girafe(ggobj=plot,
          options = list(opts_tooltip(css = tooltip_css),
                         opts_hover(css = "fill:#98C9A3;")),
          width_svg=12.5, height_svg=12.95)

x

saveWidget(x, "supremes.html", selfcontained = T)
