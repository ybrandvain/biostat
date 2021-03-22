y <- read_csv("https://whitlockschluter3e.zoology.ubc.ca/Data/chapter16/chap16q12CricketImmunitySpermViability.csv") %>%
  filter(spermViability>75) %>%
  mutate(group = factor(1:n()),
         sign = sign((spermViability - mean(spermViability)) * ( lysozyme - mean(lysozyme))),
         sign = case_when(sign == -1 ~"negative", sign == 1 ~ "positive" ))


y_poly <- bind_rows(
  # bottom left
  mutate(y, spermViability  =  case_when(spermViability  < mean(spermViability) ~ spermViability,
                                         spermViability  > mean(spermViability) ~ mean(spermViability)),
            lysozyme  =        case_when(lysozyme        < mean(lysozyme)       ~ lysozyme,
                                         lysozyme        > mean(lysozyme)       ~ mean(lysozyme))),
  #top right
  mutate(y, spermViability  =  case_when(spermViability  > mean(spermViability) ~ spermViability,
                                        spermViability   < mean(spermViability) ~ mean(spermViability)),
         lysozyme   =         case_when(lysozyme         < mean(lysozyme)       ~ lysozyme,
                                         lysozyme        > mean(lysozyme)       ~ mean(lysozyme))),
  # top left
  mutate(y, spermViability  =  case_when(spermViability  > mean(spermViability) ~ spermViability,
                                         spermViability  < mean(spermViability) ~ mean(spermViability)),
         lysozyme   =  case_when(lysozyme                > mean(lysozyme) ~ lysozyme,
                                 lysozyme                < mean(lysozyme) ~ mean(lysozyme))),
  #
  mutate(y, spermViability  =  case_when(spermViability  < mean(spermViability) ~ spermViability,
                                         spermViability  > mean(spermViability) ~ mean(spermViability)),
         lysozyme   =  case_when(lysozyme                > mean(lysozyme)       ~ lysozyme,
                                      lysozyme           < mean(lysozyme)       ~ mean(lysozyme)))
) %>%
  arrange(group)


tot_area2 <- y %>% mutate(spermViability_over_sd = spermViability / sd(spermViability),
                         lysozyme_over_sd       = lysozyme       / sd(lysozyme))%>%
  mutate(l =  (spermViability_over_sd  - mean(spermViability_over_sd ))*(lysozyme_over_sd-mean(lysozyme_over_sd))  ) %>%
  group_by(sign) %>%
  summarise(k=sum(abs(l)) )

cor_plot_a <- ggplot(y %>% mutate(spermViability_over_sd = spermViability/2.403511,
                                  lysozyme_over_sd        = lysozyme/ 1.760738),
                     aes(x = spermViability_over_sd, y = lysozyme_over_sd))+
  geom_point()+
  geom_polygon(data = y_poly %>% mutate(spermViability_over_sd = spermViability/2.403511,
                                        lysozyme_over_sd        = lysozyme/ 1.760738), alpha = .3, aes(fill= sign, group = group))+
  geom_vline(xintercept = 83.7175/ (2.403511))+
  geom_hline(yintercept = 16.5325/ 1.760738)

cor_plot_b <- tibble(x = sqrt(c(   0, pull(tot_area2[1,2]), pull(tot_area2[1,2]), 0,
                0, pull(tot_area2[2,2]), pull(tot_area2[2,2]), 0)),
       y = sqrt(c(0,0,pull(tot_area2[1,2]), pull(tot_area2[1,2]),
             0,0,pull(tot_area2[2,2]), pull(tot_area2[2,2]))),
       sign = rep(c("-","+"), each = 4)) %>%
  ggplot(aes(x = x, y = y , fill = sign, group = sign))+
  geom_polygon( alpha = .3) +
  annotate(x = .2, y =  3.3, label = "- Area = 18.90",  color = "red", geom = "text", hjust = 0, vjust = 0, size = 5)+
  annotate(x = .2, y =  1, label =   "+ Area = 06.11",  color = "blue", geom = "text", hjust = 0, vjust = 0, size = 5)+
  theme_tufte()+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
  labs(title = "Correlation", subtitle = "= (06.11 - 18.90) / (n - 1)\n= -12.79 / 39\n= -0.328")





ggplot(aes())

y <- read_csv("https://whitlockschluter3e.zoology.ubc.ca/Data/chapter16/chap16q12CricketImmunitySpermViability.csv") %>%
  filter(spermViability>75) %>%
  mutate(group = factor(1:n()),
         sign = sign((spermViability - mean(spermViability)) * ( lysozyme - mean(lysozyme))),
         sign = case_when(sign == -1 ~"negative", sign == 1 ~ "positive" ))


y_poly <- bind_rows(
  # bottom left
  mutate(y, spermViability  =  case_when(spermViability  < mean(spermViability) ~ spermViability,
                                         spermViability  > mean(spermViability) ~ mean(spermViability)),
         lysozyme  =        case_when(lysozyme        < mean(lysozyme)       ~ lysozyme,
                                      lysozyme        > mean(lysozyme)       ~ mean(lysozyme))),
  #top right
  mutate(y, spermViability  =  case_when(spermViability  > mean(spermViability) ~ spermViability,
                                         spermViability   < mean(spermViability) ~ mean(spermViability)),
         lysozyme   =         case_when(lysozyme         < mean(lysozyme)       ~ lysozyme,
                                        lysozyme        > mean(lysozyme)       ~ mean(lysozyme))),
  # top left
  mutate(y, spermViability  =  case_when(spermViability  > mean(spermViability) ~ spermViability,
                                         spermViability  < mean(spermViability) ~ mean(spermViability)),
         lysozyme   =  case_when(lysozyme                > mean(lysozyme) ~ lysozyme,
                                 lysozyme                < mean(lysozyme) ~ mean(lysozyme))),
  #
  mutate(y, spermViability  =  case_when(spermViability  < mean(spermViability) ~ spermViability,
                                         spermViability  > mean(spermViability) ~ mean(spermViability)),
         lysozyme   =  case_when(lysozyme                > mean(lysozyme)       ~ lysozyme,
                                 lysozyme           < mean(lysozyme)       ~ mean(lysozyme)))
) %>%
  arrange(group)


tot_area <- y %>%
  mutate(l =  (spermViability - mean(spermViability))*(lysozyme-mean(lysozyme))  ) %>%
  group_by(sign) %>%
  summarise(k=sum(abs(l)) )

cov_plot_a <- ggplot(y, aes(x = spermViability, y = lysozyme))+
  geom_point()+
  geom_polygon(data = y_poly, alpha = .3, aes(fill= sign, group = group))+
  geom_vline(xintercept = 83.7175)+
  labs(title = "Visualizing covariance",
       subtitle = "Area of rectangles connecting points to means",
       x = "Sperm viability (%)",
       y = expression(paste("Lysozyme activity (",mm^2,")")))+
  theme_tufte()+
  theme(legend.position = "top")

cov_plot_b <- tibble(x = sqrt(c(   0, pull(tot_area[1,2]), pull(tot_area[1,2]), 0,
                                   0, pull(tot_area[2,2]), pull(tot_area[2,2]), 0)),
                     y = sqrt(c(0,0,pull(tot_area[1,2]), pull(tot_area[1,2]),
                                0,0,pull(tot_area[2,2]), pull(tot_area[2,2]))),
                     sign = rep(c("-","+"), each = 4)) %>%
  ggplot(aes(x = x, y = y , fill = sign, group = sign))+
  geom_polygon( alpha = .3) +
  annotate(x = .5, y =  7, label = "- Area = 80.0",   color = "red", geom = "text", hjust = 0, vjust = 0, size = 5)+
  annotate(x = .5, y =  2, label = "+ Area = 25.9", color = "blue", geom = "text", hjust = 0, vjust = 0, size = 5)+
  theme_tufte()+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  #theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
  labs(title = "    Covariance as difference in area", x ="", y="",subtitle = "= (25.9 - 80.0) / (n - 1)\n= -54.1 / 39\n= -1.388")


plot_grid(cov_plot_a , cov_plot_b+theme(legend.position = "none") ,rel_widths = c(7,6))





plot1 <- ggplot(y, aes(x = spermViability, y = lysozyme))+
  geom_vline(data = . %>%
               summarise(spermViability = mean(spermViability)),
             aes(xintercept = spermViability))+
  geom_hline(data = . %>%
               summarise(lysozyme = mean(lysozyme)),
             aes(yintercept = lysozyme))+
  theme_tufte() +
  labs(x = "Sperm viability (%)", y = expression(paste("Lysozyme activity (",mm^2,")")))

tmp <- lapply(0:41, function(i){
  if(i == 0 ){
    myplot <- plot_grid(plot1 + annotate(x = .3+mean(y$spermViability), y = 18, geom = "text",label = "mean\nsperm\nviability", hjust =0,vjust = 0, size = 2)+ annotate(y = .3+mean(y$lysozyme), x = 79, geom = "text",label = "mean\nlysozyme\nactivity", hjust =0,vjust = 1.5, size =2)+ geom_point(alpha = 0) + geom_point(alpha = 0) + annotate(x = c(79,79,88.5,88.5), y = c(13,20, 13,20), color = c("blue","red","red","blue"), label = c("+","-","-","+"), geom = "text"), NULL, labels = c("A",""))
  }
  if(i == 41){
    myplot <-  plot_grid(cov_plot_a , cov_plot_b+theme(legend.position = "none") ,rel_widths = c(7,6), labels = c("A","B"))
  }
  if(i>0 & i <41){
  group_id <- i

  ugly_tmp <- y%>% mutate(xp = (spermViability - mean(spermViability)) *(lysozyme - mean(lysozyme)))
  sumtonow <- filter(ugly_tmp , as.numeric(group) < i) %>% summarise(x = sum(xp))%>% pull(x) %>% round(digits = 2)
  now      <- filter(ugly_tmp , as.numeric(group) == i) %>% summarise(x = sum(xp))%>% pull(x) %>% round(digits = 2)
  subtit <-paste(sumtonow, ifelse(sumtonow >0," + "," - "),abs(now)," = ",sumtonow + now, sep = "")
  figa <- plot1                            +
    geom_text(aes(label = group),
            size = 2,
            color = "lightgrey") +
    geom_text(data = . %>% filter(group == group_id),
            aes(label = group),
            size = 2)+
    geom_polygon(data = y_poly %>% filter(group == group_id),
               alpha = .3, aes(group = group),
               fill = ifelse(y_poly %>% filter(group == group_id)  %>% pull(sign) %>%unique() == "negative","red","blue")) +
    labs(x = "Sperm viability (%)", y = expression(paste("Lysozyme activity (",mm^2,")")),
         title = sprintf("Observation %s:",i),
         subtitle = sprintf("X = %s; Y = %s",filter(y,group == i) %>% pull(spermViability ), filter(y,group == i) %>% pull(lysozyme)))+
    geom_point(alpha = 0) + annotate(x = c(79,79,88.5,88.5), y = c(13,20, 13,20), color = c("blue","red","red","blue"), label = c("+","-","-","+"), geom = "text")


  figb <- y%>%
    mutate(a=sprintf("x[%s]=(%s - %s)(%s - %s) = %s",group , spermViability, round(mean(spermViability)), lysozyme, round(mean(lysozyme), digits = 1),
                   round((spermViability - mean(spermViability)) * ( lysozyme - mean( lysozyme)) ,
                         digits = 2)))%>%
  ggplot(aes(x = ifelse(as.numeric(group) < 21, 0,2),
             y = 20-ifelse(as.numeric(group) < 21, as.numeric(group),as.numeric(group)-20), label = a))+
  geom_text(size = 2, hjust = 0, color = "lightgrey") +
  geom_text(data = . %>% filter(group == group_id),
            size = 2, hjust = 0,
            color = ifelse(now <0 ,"red", "blue")) +
  scale_x_continuous(limits = c(0,4))+
  theme_tufte()+
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank())+
  labs(title = paste("    Running sum to obs ", i, ":", sep =""),
       subtitle = subtit)
 myplot <-  plot_grid(figa,figb, labels = c("A","B"))
}
 print(i)
 ggsave(myplot,filename =ifelse(i < 10,
            sprintf("images/covan/fig%s%s.pdf",0,i),
            sprintf("images/covan/fig%s.pdf",i)),
        width = 6.5, height = 3
          )
})


