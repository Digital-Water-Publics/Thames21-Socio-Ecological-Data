library(spacyr)
spacy_initialize(model = "en_core_web_sm")
txt = "The EU said at COP26 they want to lead global change to protect our planet We assume this means the European Parliament will #VoteThisCAPDown next week, as the proposed CAP would be disastrous to the climate and environment and is not nearly in line with the Paris Agreement."
parsed_txt = spacy_parse(txt, lemma = FALSE, entity = TRUE, nounphrase = TRUE)
entity_extract(parsed_txt)
