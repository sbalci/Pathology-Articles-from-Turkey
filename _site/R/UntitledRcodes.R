
df_gb[, 2]

df_gb[, 2] <-
  slot(
    fetched_gallbladder_articles,
    names_df_gallbladder_articles[2]
  )

for (i in 2:dim(df_gb)[2]) {
  df_gb[, i] <-
    slot(
      fetched_gallbladder_articles,
      names_df_gallbladder_articles[i]
    )
}

# nms <- slotNames(s4obj)
names_df_gallbladder_articles <-
  slotNames(fetched_gallbladder_articles)

# lst <- lapply(nms, function(nm) slot(s4obj, nm))
lst <- lapply(
  names_df_gallbladder_articles,
  function(nm) {
    slot(fetched_gallbladder_articles, nm)
  }
)

# as.data.frame(setNames(lst, nms))

lst <- setNames(lst, names_df_gallbladder_articles)








df_gb$PMID <-
  slot(
    fetched_gallbladder_articles,
    names_df_gallbladder_articles[2]
  )

slot(
  fetched_gallbladder_articles,
  names_df_gallbladder_articles[3]
)




# https://stackoverflow.com/questions/57317958/general-way-to-transform-s4-object-to-dataframe-r
# conversion function
S4_to_dataframe <- function(s4obj) {
  nms <- slotNames(s4obj)
  lst <- lapply(nms, function(nm) {
    slot(s4obj, nm)
  })
  as.data.frame(setNames(lst, nms))
}


df_gallbladder_articles <-
  S4_to_dataframe(s4obj = fetched_gallbladder_articles)
