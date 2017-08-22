library(ggplot2)

df <- read.csv('agebyregion&sex.csv', stringsAsFactors = F)
df$region <- forcats::fct_reorder2(df$region, x = -df$meanm, y = df$meanf)

png('meanagebyregion&sex.png', width = 1000, height = 1000)

ggplot(df, aes(x = region))+
  geom_linerange(aes(ymin = meanf, ymax = meanm), size = 9, color = '#e0e0e0')+
  geom_hline(yintercept = seq(23, 31, 1), size = 1.5, color = '#EFF2F4')+
  geom_point(aes(y = meanf, fill = 'female'), alpha = 0.7,
             size = 9, shape = 21, color = '#5D646F', stroke = 1.5)+
  geom_point(aes(y = meanm, fill = 'male'), alpha = 0.7,
             size = 9, shape = 21, color = '#5D646F', stroke = 1.5)+
  geom_text(aes(y = meanf - 0.25, label = region, family = 'Ubuntu Condensed'), 
            size = 6, color = '#3A3F4A', hjust = 1)+
  scale_y_continuous(position = 'top', limits = c(22, 29.5),
                     breaks = seq(23, 29, 1), 
                     labels = c('23', '24', '25', '26', rep('', 3)),
                     expand = c(0.025, 0.01), 
                     sec.axis = dup_axis(
                       labels = c('', '', '', '26', '27', '28', '29')))+
  scale_fill_manual(values = c('female' = '#b2182b', 'male' = '#2166ac'),
                    labels = c(' жінки ', ' чоловіки'))+
  
  coord_flip()+
  guides(fill = guide_legend(override.aes = list(alpha = 0.5)))+
  labs(title = 'Середній вік першого шлюбу',
       subtitle = 'В розрізі регіонів та статей.',
       caption = 'Дані: Державна служба статистики, 2015 рік | Візуалізація: Textura.in.ua')+
  theme_minimal()+
  theme(text = element_text(family = 'Ubuntu Condensed', face = 'plain', color = '#3A3F4A'),
        axis.title = element_blank(),
        axis.text = element_text(size = 18),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = c(0.335, 1.07),
        legend.direction = 'horizontal',
        panel.grid.major = element_line(linetype = 'dotted', size = 0.35, color = '#5D646F'),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = 'bold', size = 44, margin = margin(b = 20, t = 10)),
        plot.subtitle = element_text(size = 19, face = 'plain', margin = margin(b = 20)),
        plot.caption = element_text(size = 16, margin = margin(b = 20, t = 30), color = '#5D646F'),
        plot.background = element_rect(fill = '#EFF2F4'),
        plot.margin = unit(c(2, 2, 2, 2), 'cm'))

dev.off()