# wordbank.stanford.edu
require("wordbankr")
# https://github.com/langcog/wordbankr
# http://langcog.github.io/wordbankr/
instrument <- get_instruments()
# e.g., instrument$form might be WG, WS, TC, Oxford CDI...
# what are TEDS Twos/Threes, FormA, FormBOne, FormBTwo, and FormC?

# x administration
english_ws_admins <- get_administration_data(language="English (American)", form="WS") # 5520 subjects (with age etc) 1306 longitudinal
english_br_ocdi_admins <- get_administration_data(language="English (British)", form="Oxford CDI") # 1210, 910 longitudinal
english_au_ws_admins <- get_administration_data(language="English (Australian)", form="WS") # 1497, 0 longitudinal

# Words and Gestures
english_ws_admins <- get_administration_data(language="English (American)", form="WS") # 5520 subjects (with age etc) 1306 longitudinal
english_br_wg_admins <- get_administration_data(language="English (British)", form="WG")
english_au_wg_admins <- get_administration_data(language="English (Australian)", form="WG")


# what is comprehensions and production number? (e.g., 497, 369...)
all_admins <- get_administration_data()

# x item
english_ws_items <- get_item_data(language="English", form="WS")
english_br_ws_items <- get_item_data("English", "Oxford CDI")
all_items <- get_item_data()

# admin x item
en_ws <- get_instrument_data("English", "WS") # 4.6m rows
length(unique(en_ws$num_item_id)) # 797 items?
length(unique(en_ws$data_id)) # 5776 subjects?

#save(en_ws, file="")
require("tidyverse")

animals <- get_item_data(language = "English (American)", form = "WS") %>%
  filter(category == "animals")

animal_data <- get_instrument_data(language = "English (American)",
                                   form = "WS",
                                   items = animals$item_id,
                                   administrations = TRUE)

animal_summary <- animal_data %>%
  mutate(produces = value == "produces") %>%
  group_by(age, data_id) %>%
  summarise(num_animals = sum(produces, na.rm = TRUE)) %>%
  group_by(age) %>%
  summarise(median_num_animals = median(num_animals, na.rm = TRUE))
  
ggplot(animal_summary, aes(x = age, y = median_num_animals)) +
  geom_point() +
  labs(x = "Age (months)", y = "Median animal words producing")


eng_ws_data <- get_instrument_data(language = "English (American)",
                                   form = "WS",
                                   items = c("item_1", "item_42"),
                                   administrations = TRUE,
                                   iteminfo = TRUE)
fit_aoa(eng_ws_data)

en_wg <- get_instrument_data(language="English (American)", form="WG") # 1.2m rows
length(unique(en_wg$data_id)) # 2454 subjects?

# estimating age of acquisition:
# http://mikabr.github.io/aoa-prediction/aoa_estimation.html

encdi = read.csv("ENitem_data_prop_producing_OxfordCDI.csv", header=T)

enws = read.csv("ENitem_data_prop_producing_WS.csv", header=T)
