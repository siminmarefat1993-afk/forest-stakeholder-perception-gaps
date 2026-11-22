packages <- c(
  "readxl",
  "dplyr",
  "tidyr",
  "ggplot2",
  "writexl"
)

results <- sapply(packages, function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
})

print(results)

library(readxl)

file_path <- file.choose()

file.exists(file_path)
library(readxl)

Data_1 <- read_excel(file_path)

dim(Data_1)
names(Data_1)
head(Data_1, 3)

# 1. بررسی تعداد پاسخ‌دهندگان هر گروه
table(Data_1$Groups, useNA = "ifany")

# 2. بررسی داده‌های گمشده در هر ستون
colSums(is.na(Data_1))

# 3. بررسی مقادیر موجود در ستون گروه
sort(unique(Data_1$Groups))

# 4. بررسی مقادیر مجاز در 16 گویه
items <- paste0("C", 1:16)

lapply(Data_1[items], function(x) {
  sort(unique(x))
})

# 5. بررسی خلاصه آماری گویه‌ها
summary(Data_1[items])

# تعداد افراد در هر گروه
table(Data_1$Groups, useNA = "ifany")

# داده‌های گمشده
colSums(is.na(Data_1))

# کدهای گروه‌ها
sort(unique(Data_1$Groups))

# بررسی مقادیر خارج از بازه 1 تا 5
items <- paste0("C", 1:16)

sapply(Data_1[items], function(x) {
  sum(!is.na(x) & !(x %in% 1:5))
})

# آزمون کروسکال والیس برای 16 تعارض

items <- paste0("C", 1:16)

kw_results <- lapply(items, function(item) {
  
  formula <- as.formula(
    paste(item, "~ factor(Groups)")
  )
  
  test <- kruskal.test(
    formula,
    data = Data_1
  )
  
  data.frame(
    Conflict = item,
    N = sum(complete.cases(Data_1[, c(item, "Groups")])),
    H = unname(test$statistic),
    df = unname(test$parameter),
    p_value = test$p.value
  )
})

kw_results <- do.call(rbind, kw_results)

# اصلاح نمایش اعداد
kw_results$p_adjusted_BH <- p.adjust(
  kw_results$p_value,
  method = "BH"
)

print(kw_results, digits = 6)

# ذخیره نتایج در اکسل
writexl::write_xlsx(
  kw_results,
  "Kruskal_Wallis_Results.xlsx"
)

# مقایسه‌های زوجی من ویتنی
items <- paste0("C", 1:16)

group_pairs <- combn(sort(unique(Data_1$Groups)), 2,
                     simplify = FALSE)

pair_results <- list()
k <- 1

for (item in items) {
  for (pair in group_pairs) {
    
    g1 <- pair[1]
    g2 <- pair[2]
    
    x <- na.omit(Data_1[Data_1$Groups == g1, item][[1]])
    y <- na.omit(Data_1[Data_1$Groups == g2, item][[1]])
    
    test <- wilcox.test(
      x, y,
      exact = FALSE,
      correct = FALSE
    )
    
    n1 <- length(x)
    n2 <- length(y)
    
    # محاسبه Z با استفاده از مقدار احتمال دوطرفه
    z_abs <- qnorm(test$p.value / 2, lower.tail = FALSE)
    
    # جهت اختلاف بر اساس میانگین رتبه‌ها
    pooled <- c(x, y)
    ranks <- rank(pooled, ties.method = "average")
    
    rank1 <- mean(ranks[seq_len(n1)])
    rank2 <- mean(ranks[n1 + seq_len(n2)])
    
    z_signed <- if (rank1 >= rank2) z_abs else -z_abs
    
    pair_results[[k]] <- data.frame(
      Conflict = item,
      Group1 = g1,
      Group2 = g2,
      N1 = n1,
      N2 = n2,
      MeanRank1 = rank1,
      MeanRank2 = rank2,
      U = unname(test$statistic),
      p_raw = test$p.value,
      Z_signed = z_signed,
      Effect_r = abs(z_signed) / sqrt(n1 + n2)
    )
    
    k <- k + 1
  }
}

pair_results <- do.call(rbind, pair_results)

# اصلاح جداگانه در هر تعارض، برای شش مقایسه
pair_results$p_bonferroni <- ave(
  pair_results$p_raw,
  pair_results$Conflict,
  FUN = function(p) p.adjust(p, method = "bonferroni")
)

pair_results$p_holm <- ave(
  pair_results$p_raw,
  pair_results$Conflict,
  FUN = function(p) p.adjust(p, method = "holm")
)

# نمایش نتایج
print(pair_results, digits = 6)

# ذخیره در فایل اکسل
writexl::write_xlsx(
  pair_results,
  "Pairwise_Mann_Whitney_Results.xlsx"
)

items <- paste0("C", 1:16)
group_pairs <- combn(sort(unique(Data_1$Groups)), 2,
                     simplify = FALSE)
stopifnot(length(unique(Data_1$Groups)) == 4)

pair_results <- list()
k <- 1

for (item in items) {
  for (pair in group_pairs) {
    
    g1 <- pair[1]
    g2 <- pair[2]
    
    x <- na.omit(Data_1[Data_1$Groups == g1, item][[1]])
    y <- na.omit(Data_1[Data_1$Groups == g2, item][[1]])
    
    n1 <- length(x)
    n2 <- length(y)
    pooled <- c(x, y)
    ranks <- rank(pooled, ties.method = "average")
    N <- length(pooled)
    
    rank1 <- mean(ranks[seq_len(n1)])
    rank2 <- mean(ranks[n1 + seq_len(n2)])
    
    test <- wilcox.test(
      x, y,
      exact = FALSE,
      correct = FALSE
    )
    
    # آماره U متناظر با گروه اول
    U <- unname(test$statistic)
    
    # تصحیح واریانس برای رتبه‌های مساوی
    tie_counts <- table(pooled)
    tie_term <- sum(tie_counts^3 - tie_counts)
    
    var_U <- (n1 * n2 / 12) *
      ((N + 1) - tie_term / (N * (N - 1)))
    if (!is.finite(var_U) || var_U <= 0) {
      stop("Invalid variance for item: ", item, " and groups: ", g1, " vs ", g2)
    }
    
    z_abs <- abs(U - (n1 * n2 / 2)) / sqrt(var_U)
    z_signed <- if (rank1 >= rank2) z_abs else -z_abs
    
    pair_results[[k]] <- data.frame(
      Conflict = item,
      Group1 = g1,
      Group2 = g2,
      N1 = n1,
      N2 = n2,
      MeanRank1 = rank1,
      MeanRank2 = rank2,
      U = U,
      p_raw = test$p.value,
      Z_signed = z_signed,
      Effect_r = z_abs / sqrt(n1 + n2)
    )
    
    k <- k + 1
  }
}

pair_results <- do.call(rbind, pair_results)
stopifnot(nrow(pair_results) == 96)

pair_results$p_bonferroni <- ave(
  pair_results$p_raw,
  pair_results$Conflict,
  FUN = function(p) p.adjust(p, method = "bonferroni")
)

pair_results$p_holm <- ave(
  pair_results$p_raw,
  pair_results$Conflict,
  FUN = function(p) p.adjust(p, method = "holm")
)

writexl::write_xlsx(
  pair_results,
  "Pairwise_Mann_Whitney_Results_Corrected.xlsx"
)

# کنترل تعداد مقایسه‌ها
nrow(pair_results)

# تعداد مقایسه‌های معنادار پس از هر اصلاح
sum(pair_results$p_bonferroni < 0.05)
sum(pair_results$p_holm < 0.05)


group_summary <- aggregate(
  cbind(
    Bonferroni_Significant = pair_results$p_bonferroni < 0.05,
    Holm_Significant = pair_results$p_holm < 0.05
  ),
  by = list(
    Group1 = pair_results$Group1,
    Group2 = pair_results$Group2
  ),
  FUN = sum
)

print(group_summary, row.names = FALSE)


holm_only <- subset(
  pair_results,
  p_holm < 0.05 & p_bonferroni >= 0.05
)

holm_only <- holm_only[
  order(holm_only$p_holm),
  c(
    "Conflict", "Group1", "Group2",
    "MeanRank1", "MeanRank2",
    "p_raw", "p_bonferroni", "p_holm", "Effect_r"
  )
]

print(holm_only, row.names = FALSE, digits = 5)


# نام گروه‌ها
group_labels <- c(
  "1" = "مردم محلی",
  "2" = "کارشناسان دولتی",
  "3" = "سازمان‌های مردم‌نهاد",
  "4" = "دانشگاهیان و پژوهشگران"
)

# نام تعارض‌ها
conflict_labels <- c(
  "C1" = "استخراج و اکتشاف معدن",
  "C2" = "حضور دام",
  "C3" = "بهره‌برداری محصولات فرعی جنگل",
  "C4" = "بهره‌برداری چوب",
  "C5" = "بهره‌برداری غیرقانونی محصولات فرعی",
  "C6" = "بهره‌برداری از منابع آب جنگل",
  "C7" = "شکار",
  "C8" = "تعارض بین جنگل‌نشینان و ساکنان روستاهای همجوار",
  "C9" = "تعارض بین تحقیقات و حفظ اکوسیستم",
  "C10" = "تعارض در مدیریت اراضی",
  "C11" = "توسعه اراضی کشاورزی",
  "C12" = "تولید هیزم",
  "C13" = "بیشه‌زراعی",
  "C14" = "احداث جاده‌های دسترسی",
  "C15" = "احداث بزرگراه یا جاده‌های درجه یک",
  "C16" = "توسعه گردشگری"
)

# آماده‌سازی جدول
results_sorted <- pair_results

results_sorted$Group1_Name <- unname(
  group_labels[as.character(results_sorted$Group1)]
)
results_sorted$Group2_Name <- unname(
  group_labels[as.character(results_sorted$Group2)]
)
results_sorted$Conflict_Name <- unname(
  conflict_labels[results_sorted$Conflict]
)

# جهت اختلاف بر اساس میانگین رتبه
results_sorted$Higher_Rank_Group <- ifelse(
  results_sorted$MeanRank1 > results_sorted$MeanRank2,
  results_sorted$Group1_Name,
  ifelse(
    results_sorted$MeanRank2 > results_sorted$MeanRank1,
    results_sorted$Group2_Name,
    "میانگین رتبه برابر"
  )
)

# وضعیت معناداری
results_sorted$Bonferroni_Significant <-
  results_sorted$p_bonferroni < 0.05

results_sorted$Holm_Significant <-
  results_sorted$p_holm < 0.05

# مرتب‌سازی بر اساس اندازه اثر
results_sorted <- results_sorted[
  order(-results_sorted$Effect_r),
]

# انتخاب ستون‌های گزارش
results_sorted <- results_sorted[, c(
  "Conflict", "Conflict_Name",
  "Group1_Name", "Group2_Name",
  "MeanRank1", "MeanRank2",
  "Higher_Rank_Group", "Effect_r",
  "p_raw", "p_bonferroni", "p_holm",
  "Bonferroni_Significant", "Holm_Significant"
)]

# مشاهده جدول
print(results_sorted, row.names = FALSE, digits = 4)

# ذخیره فایل اکسل
writexl::write_xlsx(
  results_sorted,
  "All_96_Pairwise_Comparisons_Sorted.xlsx"
)



bonf_significant <- subset(
  results_sorted,
  Bonferroni_Significant
)

bonf_significant <- bonf_significant[
  order(-bonf_significant$Effect_r),
]

print(
  bonf_significant[, c(
    "Conflict", "Conflict_Name",
    "Group1_Name", "Group2_Name",
    "MeanRank1", "MeanRank2",
    "Higher_Rank_Group", "Effect_r",
    "p_bonferroni", "p_holm"
  )],
  row.names = FALSE,
  digits = 4
)

writexl::write_xlsx(
  bonf_significant,
  "Bonferroni_Significant_Comparisons.xlsx"
)


top_significant <- subset(
  results_sorted,
  Bonferroni_Significant
)

top_significant <- head(
  top_significant[order(-top_significant$Effect_r), ],
  15
)

print(
  top_significant[, c(
    "Conflict", "Conflict_Name",
    "Group1_Name", "Group2_Name",
    "MeanRank1", "MeanRank2",
    "Higher_Rank_Group", "Effect_r",
    "p_bonferroni", "p_holm"
  )],
  row.names = FALSE,
  digits = 4
)

writexl::write_xlsx(
  top_significant,
  "Top_15_Significant_Effects.xlsx"
)


library(dplyr)
library(writexl)

# تعداد مقایسه‌های معنادار برای هر تعارض
summary_counts <- results_sorted %>%
  group_by(Conflict, Conflict_Name) %>%
  summarise(
    Total_Comparisons = n(),
    Significant_Comparisons = sum(Bonferroni_Significant, na.rm = TRUE),
    .groups = "drop"
  )

# بزرگ‌ترین اندازه اثر در میان مقایسه‌های معنادار هر تعارض
strongest_pairs <- results_sorted %>%
  filter(Bonferroni_Significant) %>%
  group_by(Conflict, Conflict_Name) %>%
  slice_max(
    order_by = Effect_r,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup() %>%
  select(
    Conflict,
    Conflict_Name,
    Group1_Name,
    Group2_Name,
    Higher_Rank_Group,
    Effect_r,
    p_bonferroni,
    p_holm
  ) %>%
  rename(
    Strongest_Group1 = Group1_Name,
    Strongest_Group2 = Group2_Name,
    Group_With_Higher_Rank = Higher_Rank_Group,
    Strongest_Effect_r = Effect_r,
    Strongest_p_Bonferroni = p_bonferroni,
    Strongest_p_Holm = p_holm
  )

# ترکیب دو جدول
conflict_summary <- summary_counts %>%
  left_join(
    strongest_pairs,
    by = c("Conflict", "Conflict_Name")
  ) %>%
  arrange(
    desc(Significant_Comparisons),
    desc(Strongest_Effect_r)
  )

# نمایش جدول
print(conflict_summary, n = Inf, width = Inf)

# ذخیره در فایل اکسل
write_xlsx(
  conflict_summary,
  "Conflict_Level_Summary.xlsx"
)


library(dplyr)
library(ggplot2)

# تعیین ترتیب 16 تعارض
conflict_order <- paste0("C", 1:16)

# آماده‌سازی داده‌های نقشه حرارتی
heatmap_data <- results_sorted %>%
  mutate(
    Conflict = factor(Conflict, levels = rev(conflict_order)),
    
    Pair = paste(Group1_Name, Group2_Name, sep = " | "),
    
    Significance = p_bonferroni < 0.05,
    
    Direction = case_when(
      !Significance ~ "غیرمعنادار",
      MeanRank1 > MeanRank2 ~ paste0(Group1_Name, " رتبه بالاتر"),
      MeanRank2 > MeanRank1 ~ paste0(Group2_Name, " رتبه بالاتر"),
      TRUE ~ "رتبه برابر"
    ),
    
    Cell_Label = ifelse(
      Significance,
      paste0(sprintf("%.2f", Effect_r), "*"),
      ""
    )
  )

# ترتیب ثابت شش مقایسه
pair_order <- c(
  "مردم محلی | کارشناسان دولتی",
  "مردم محلی | سازمان‌های مردم‌نهاد",
  "مردم محلی | دانشگاهیان و پژوهشگران",
  "کارشناسان دولتی | سازمان‌های مردم‌نهاد",
  "کارشناسان دولتی | دانشگاهیان و پژوهشگران",
  "سازمان‌های مردم‌نهاد | دانشگاهیان و پژوهشگران"
)

heatmap_data <- heatmap_data %>%
  mutate(Pair = factor(Pair, levels = pair_order))


library(dplyr)
library(ggplot2)

conflict_labels_en <- c(
  C1 = "Mining exploration and extraction",
  C2 = "Livestock presence",
  C3 = "Non-timber forest product harvesting",
  C4 = "Timber harvesting",
  C5 = "Illegal non-timber forest product harvesting",
  C6 = "Forest water use",
  C7 = "Hunting",
  C8 = "Conflict between forest dwellers and residents of adjacent villages",
  C9 = "Conflict between research and ecosystem conservation",
  C10 = "Land management conflict",
  C11 = "Agricultural land expansion",
  C12 = "Firewood production",
  C13 = "Silvopasture",
  C14 = "Access road construction",
  C15 = "Highway and primary road construction",
  C16 = "Tourism development"
)

pair_order <- c(
  "Local residents | Government experts",
  "Local residents | NGOs",
  "Local residents | Academics and researchers",
  "Government experts | NGOs",
  "Government experts | Academics and researchers",
  "NGOs | Academics and researchers"
)

heatmap_data_en <- results_sorted %>%
  mutate(
    Conflict = factor(
      Conflict,
      levels = rev(paste0("C", 1:16)),
      labels = rev(conflict_labels_en[paste0("C", 1:16)])
    ),
    Group1_EN = recode(
      Group1_Name,
      "مردم محلی" = "Local residents",
      "کارشناسان دولتی" = "Government experts",
      "اعضای سازمان‌های مردم‌نهاد" = "NGOs",
      "سازمان‌های مردم‌نهاد" = "NGOs",
      "دانشگاهیان و پژوهشگران" = "Academics and researchers"
    ),
    Group2_EN = recode(
      Group2_Name,
      "مردم محلی" = "Local residents",
      "کارشناسان دولتی" = "Government experts",
      "اعضای سازمان‌های مردم‌نهاد" = "NGOs",
      "سازمان‌های مردم‌نهاد" = "NGOs",
      "دانشگاهیان و پژوهشگران" = "Academics and researchers"
    ),
    Pair = paste(Group1_EN, Group2_EN, sep = " | "),
    Significant = p_bonferroni < 0.05,
    Direction = case_when(
      !Significant ~ "Not significant",
      MeanRank1 > MeanRank2 ~ paste(Group1_EN, "higher rank"),
      MeanRank2 > MeanRank1 ~ paste(Group2_EN, "higher rank"),
      TRUE ~ "Equal ranks"
    ),
    Cell_Label = ifelse(
      Significant,
      paste0(sprintf("%.2f", Effect_r), "*"),
      ""
    ),
    Pair = factor(Pair, levels = pair_order)
  )

p_heatmap_en <- ggplot(
  heatmap_data_en,
  aes(x = Pair, y = Conflict, fill = Direction)
) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = Cell_Label), size = 3.0) +
  scale_fill_manual(
    values = c(
      "Not significant" = "grey85",
      "Local residents higher rank" = "#2878B5",
      "Government experts higher rank" = "#D95F02",
      "NGOs higher rank" = "#1B9E77",
      "Academics and researchers higher rank" = "#8E63B6",
      "Equal ranks" = "grey85"
    ),
    name = "Pairwise comparison result"
  ) +
  labs(
    title = "Heatmap of Perceptual Gaps Among Stakeholder Groups",
    subtitle = paste(
      "Asterisks indicate significance after Bonferroni correction;",
      "numbers show effect size (r)"
    ),
    x = "Stakeholder group comparison",
    y = "Conflict"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_text(
      angle = 40, hjust = 0.5, vjust = 1
    ),
    axis.text.y = element_text(size = 22),
    panel.grid = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot"
  )

print(p_heatmap_en)

ggsave(
  "Perception_Gap_Heatmap_English.png",
  plot = p_heatmap_en,
  width = 14, height = 9, dpi = 300, bg = "white"
)

ggsave(
  "Perception_Gap_Heatmap_English.pdf",
  plot = p_heatmap_en,
  width = 14, height = 9,
  device = cairo_pdf, bg = "white"
)

names(results_sorted)

library(dplyr)

heatmap_data_en <- results_sorted %>%
  mutate(
    Group1_EN = recode(
      Group1_Name,
      "مردم محلی" = "Local residents",
      "کارشناسان دولتی" = "Government experts",
      "اعضای سازمان‌های مردم‌نهاد" = "NGOs",
      "سازمان‌های مردم‌نهاد" = "NGOs",
      "دانشگاهیان و پژوهشگران" = "Academics and researchers"
    ),
    Group2_EN = recode(
      Group2_Name,
      "مردم محلی" = "Local residents",
      "کارشناسان دولتی" = "Government experts",
      "اعضای سازمان‌های مردم‌نهاد" = "NGOs",
      "سازمان‌های مردم‌نهاد" = "NGOs",
      "دانشگاهیان و پژوهشگران" = "Academics and researchers"
    ),
    Significant = p_bonferroni < 0.05,
    Direction = case_when(
      !Significant ~ "Not significant",
      MeanRank1 > MeanRank2 ~ paste(Group1_EN, "higher rank"),
      MeanRank2 > MeanRank1 ~ paste(Group2_EN, "higher rank"),
      TRUE ~ "Equal ranks"
    ),
    Cell_Label = ifelse(
      Significant,
      sprintf("%.2f*", Effect_r),
      ""
    )
  )



conflict_labels <- c(
  "C1"  = "استخراج و اکتشاف معدن",
  "C2"  = "حضور دام",
  "C3"  = "بهره‌برداری محصولات فرعی جنگل",
  "C4"  = "بهره‌برداری چوب",
  "C5"  = "بهره‌برداری غیرقانونی محصولات فرعی",
  "C6"  = "بهره‌برداری از منابع آب جنگل",
  "C7"  = "شکار",
  "C8"  = "تعارض جنگل‌نشینان و ساکنان روستاهای همجوار",
  "C9"  = "تعارض تحقیقات و حفظ اکوسیستم",
  "C10" = "تعارض در مدیریت اراضی",
  "C11" = "توسعه اراضی کشاورزی",
  "C12" = "تولید هیزم",
  "C13" = "بیشه‌زراعی",
  "C14" = "احداث جاده‌های دسترسی",
  "C15" = "احداث بزرگراه و جاده‌های درجه یک",
  "C16" = "توسعه گردشگری"
)




ggplot2::ggsave(
  filename = "Perception_Gap_Heatmap.png",
  plot = p_heatmap,
  width = 15,
  height = 10,
  dpi = 300,
  bg = "white"
)

ggplot2::ggsave(
  filename = "Perception_Gap_Heatmap.pdf",
  plot = p_heatmap,
  width = 15,
  height = 10,
  bg = "white"
)


getwd()
file.exists("Perception_Gap_Heatmap.png")
file.exists("Perception_Gap_Heatmap.pdf")

shell.exec(
  normalizePath(
    file.path(getwd(), "Perception_Gap_Heatmap.png")
  )
)



library(ggplot2)
library(dplyr)

# بررسی وجود نتایج تحلیل
stopifnot(exists("results_sorted"))

# نام گروه‌ها را به برچسب‌های انگلیسی شکل تبدیل می‌کنیم
group_labels <- c(
  "مردم محلی" = "Local residents",
  "کارشناسان دولتی" = "Government experts",
  "سازمان‌های مردم‌نهاد" = "NGO members",
  "دانشگاهیان و پژوهشگران" = "Academics"
)

# ترتیب شش مقایسه زوجی
pair_order <- c(
  "Local residents\nvs government\nexperts",
  "Local residents\nvs NGO\nmembers",
  "Local residents\nvs academics",
  "Government\nexperts vs NGO\nmembers",
  "Government experts\nvs academics",
  "NGO members\nvs academics"
)

# آماده‌سازی داده‌ها

# آماده‌سازی داده‌ها
heatmap_data <- results_sorted %>%
  mutate(
    Pair = paste(Group1_Name, Group2_Name, sep = " | "),
    Pair = factor(Pair, levels = pair_order),
    Conflict = factor(
      as.character(Conflict),
      levels = rev(paste0("C", 1:16))
    ),
    Significant = !is.na(p_bonferroni) &
      p_bonferroni < 0.05,
    Plot_Effect = ifelse(
      Significant,
      Effect_r,
      NA_real_
    ),
    Cell_Label = ifelse(
      Significant,
      paste0(sprintf("%.2f", Effect_r), "*"),
      ""
    )
  )


# ============================================================
# FINAL ENGLISH PUBLICATION-READY HEATMAP
# ============================================================

library(ggplot2)
library(showtext)
library(sysfonts)
library(ragg)

# ------------------------------------------------------------
# 1. Times New Roman font
# ------------------------------------------------------------

font_regular <- "C:/Windows/Fonts/times.ttf"
font_bold <- "C:/Windows/Fonts/timesbd.ttf"

if (!file.exists(font_regular) || !file.exists(font_bold)) {
  stop("Times New Roman font files were not found.")
}

sysfonts::font_add(
  family = "Times New Roman",
  regular = font_regular,
  bold = font_bold
)

showtext::showtext_auto(enable = FALSE)

# ------------------------------------------------------------
# 2. English stakeholder comparison labels
# ------------------------------------------------------------

pair_order <- c(
  "مردم محلی | کارشناسان دولتی",
  "مردم محلی | سازمان‌های مردم‌نهاد",
  "مردم محلی | دانشگاهیان و پژوهشگران",
  "کارشناسان دولتی | سازمان‌های مردم‌نهاد",
  "کارشناسان دولتی | دانشگاهیان و پژوهشگران",
  "سازمان‌های مردم‌نهاد | دانشگاهیان و پژوهشگران"
)

pair_labels <- c(
  "Local residents\nvs government\nexperts",
  "Local residents\nvs NGO\nmembers",
  "Local residents\nvs academics",
  "Government\nexperts vs NGO\nmembers",
  "Government experts\nvs academics",
  "NGO members\nvs academics"
)

# ------------------------------------------------------------
# 3. Check existing plotting data
# No statistical calculations are performed here.
# ------------------------------------------------------------

required_columns <- c(
  "Pair",
  "Conflict",
  "Plot_Effect",
  "Cell_Label"
)

if (!all(required_columns %in% names(heatmap_data))) {
  stop("Required columns are missing from heatmap_data.")
}

heatmap_data$Pair <- factor(
  as.character(heatmap_data$Pair),
  levels = pair_order
)

heatmap_data$Conflict <- factor(
  as.character(heatmap_data$Conflict),
  levels = rev(paste0("C", 1:16))
)

# ------------------------------------------------------------
# 4. Create the English heatmap
# ------------------------------------------------------------

p_publication <- ggplot(
  heatmap_data,
  aes(
    x = Pair,
    y = Conflict,
    fill = Plot_Effect
  )
) +
  
  geom_tile(
    color = "white",
    linewidth = 0.45
  ) +
  
  geom_text(
    aes(label = Cell_Label),
    family = "Times New Roman",
    size = 12,
    color = "black"
  ) +
  
  scale_x_discrete(
    limits = pair_order,
    labels = setNames(pair_labels, pair_order),
    drop = FALSE
  ) +
  
  scale_y_discrete(
    limits = rev(paste0("C", 1:16)),
    drop = FALSE
  ) +
  
  scale_fill_viridis_c(
    option = "viridis",
    limits = c(0, 0.75),
    breaks = seq(0, 0.7, by = 0.1),
    na.value = "white",
    name = "Effect size (r)"
  ) +
  
  labs(
    title = paste(
      "Heat map of stakeholder perception-gap",
      "effect sizes after Bonferroni correction"
    ),
    x = "Stakeholder-pair comparisons",
    y = "Conflict issue code"
  ) +
  
  theme_minimal(
    base_family = "Times New Roman",
    base_size = 20
  ) +
  
  theme(
    text = element_text(
      family = "Times New Roman",
      color = "black"
    ),
    
    plot.title = element_text(
      face = "bold",
      size = 30,
      hjust = 0.5,
      color = "black"
    ),
    
    plot.title.position = "plot",
    
    axis.title.x = element_text(
      size = 22,
      margin = margin(t = 22)
    ),
    
    axis.title.y = element_text(
      size = 22,
      margin = margin(r = 10)
    ),
    
    axis.text.x = element_text(
      size = 22,
      angle = 25,
      hjust = 1,
      vjust = 1,
      lineheight = 1.05
    ),
    
    axis.text.y = element_text(
      size = 22
    ),
    
    legend.title = element_text(size = 22),
    legend.text = element_text(size = 19),
    
    legend.position = "right",
    
    panel.grid = element_blank(),
    
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    plot.margin = margin(12, 18, 18, 12)
  )

# ------------------------------------------------------------
# 5. Display the final English heatmap
# ------------------------------------------------------------

print(p_publication)

# ------------------------------------------------------------
# 6. Save publication files in project folder
# ------------------------------------------------------------

output_dir <- file.path(getwd(), "figures")

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# PNG - 300 dpi
ggsave(
  filename = file.path(
    output_dir,
    "Stakeholder_Perception_Gap_Heatmap.png"
  ),
  plot = p_publication,
  device = ragg::agg_png,
  width = 16.38,
  height = 9.20,
  units = "in",
  dpi = 300,
  background = "white"
)

# TIFF - 600 dpi
ggsave(
  filename = file.path(
    output_dir,
    "Stakeholder_Perception_Gap_Heatmap.tiff"
  ),
  plot = p_publication,
  device = ragg::agg_tiff,
  width = 16.38,
  height = 9.20,
  units = "in",
  dpi = 600,
  compression = "lzw",
  background = "white"
)

# PDF
ggsave(
  filename = file.path(output_dir, "Stakeholder_Perception_Gap_Heatmap.png"),
  plot = p_publication,
  device = ragg::agg_png,
  width = 16.38, height = 9.20, units = "in",
  bg = "white"
)

# ------------------------------------------------------------
# 7. Verify exported files
# ------------------------------------------------------------

print(list.files(
  output_dir,
  pattern = "Stakeholder_Perception_Gap_Heatmap",
  full.names = TRUE
))
