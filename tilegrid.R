library(dplyr)
library(ggplot2)

df <- read.csv('agebyregion&sex.csv', stringsAsFactors = F)

ukraine <- data.frame(
  
  row = c(1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 5, 5),
  
  col = c(2, 5, 6, 7, 1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 5, 6, 7, 8, 2, 5, 6, 7, 4, 6, 5),
  
  code = c(1:27),
  
  name = as.character(c("Волинська", "Київ", "Чернігівська", "Сумська", "Львівська", "Тернопільська", "Рівненська", "Житомирська", "Київська", "Полтавська", "Харківська", "Луганська", "Закарпатська", "Івано-Франківська", "Хмельницька", "Вінницька", "Черкаська", "Кіровоградська", "Дніпропетровська", "Донецька", "Чернівецька", "Миколаївська", "Херсонська", "Запорізька", "Одеська", "АР Крим", "м. Севастополь")),
  
  stringsAsFactors = F
  
)

ukraine <- ukraine %>% 
  inner_join(df, by = c('name' = 'region')) %>% 
  select(-code, -medianf, -medianm) %>% 
  tidyr::gather(sex, age, -name, -row, -col) %>% 
  mutate(sex = forcats::fct_recode(sex, чоловіки = 'meanm', жінки = 'meanf'))

png('tilegrid_mean.png', width = 1000, height = 1000)

ggplot(ukraine)+
  geom_tile(aes(x = col, y = row, fill = age), color = '#5D646F', size = 0.05, alpha = 0.8)+
  geom_text(aes(x = col, y = row, 
                label = paste(name, age, sep = '\n'), family = 'Ubuntu Condensed'), 
            color = '#3A3F4A', size = 4.5)+
  scale_y_reverse()+
  scale_fill_distiller(palette = 4, direction = 1)+
  facet_wrap(~sex, ncol = 1)+
  labs(title = 'Середній вік першого шлюбу',
       subtitle = 'В розрізі регіонів та статей',
       caption = 'Дані: Державна служба статистики, 2015 рік | Візуалізація: Textura.in.ua')+
  theme_void()+
  theme(text = element_text(family = 'Ubuntu Condensed', face = 'plain', color = '#3A3F4A'),
        legend.position = 'none',
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 18, face = 'bold'),
        plot.title = element_text(face = 'bold', size = 44, margin = margin(b = 20, t = 10)),
        plot.subtitle = element_text(size = 19, face = 'plain', margin = margin(b = 20)),
        plot.caption = element_text(size = 16, margin = margin(b = 20, t = 30), color = '#5D646F'),
        plot.background = element_rect(fill = '#EFF2F4'),
        plot.margin = unit(c(2, 2, 2, 2), 'cm'))

dev.off()
