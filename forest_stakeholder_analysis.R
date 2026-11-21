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
________________________________________
library(readxl)

file_path <- "J:/Ph.D/مقاله اول مستخرج از رساله/New folder/Dr. Hosseinzadeh/New folder/R/Data 1.xlsx"

file.exists(file_path)
________________________________________
file_path <- "J:/Ph.D/مقاله اول مستخرج از رساله/New folder/Dr. Hosseinzadeh/New folder/R/Data 1.xlsx"

file.exists(file_path)
________________________________________
library(readxl)

Data_1 <- read_excel(file_path)

dim(Data_1)
names(Data_1)
head(Data_1, 3)
________________________________________
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
________________________________________
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
________________________________________
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
    N = nrow(Data_1),
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
________________________________________
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
    
    x <- Data_1[Data_1$Groups == g1, item][[1]]
    y <- Data_1[Data_1$Groups == g2, item][[1]]
    
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
________________________________________
items <- paste0("C", 1:16)
group_pairs <- combn(sort(unique(Data_1$Groups)), 2,
                     simplify = FALSE)

pair_results <- list()
k <- 1

for (item in items) {
  for (pair in group_pairs) {
    
    g1 <- pair[1]
    g2 <- pair[2]
    
    x <- Data_1[Data_1$Groups == g1, item][[1]]
    y <- Data_1[Data_1$Groups == g2, item][[1]]
    
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
    
    z_abs <- abs(U - n1 * n2 / 2) / sqrt(var_U)
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
________________________________________

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
________________________________________

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
________________________________________

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
________________________________________


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
________________________________________

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
________________________________________

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
________________________________________

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

# رسم نقشه حرارتی
p_heatmap <- ggplot(
  heatmap_data,
  aes(x = Pair, y = Conflict, fill = Direction)
) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(
    aes(label = Cell_Label),
    size = 3.2
  ) +
  scale_fill_manual(
    values = c(
      "غیرمعنادار" = "grey85",
      "مردم محلی رتبه بالاتر" = "#2878B5",
      "کارشناسان دولتی رتبه بالاتر" = "#D95F02",
      "سازمان‌های مردم‌نهاد رتبه بالاتر" = "#1B9E77",
      "دانشگاهیان و پژوهشگران رتبه بالاتر" = "#8E63B6",
      "رتبه برابر" = "grey85"
    ),
    name = "نتیجه مقایسه",
    na.value = "grey85"
  ) +
  labs(
    title = "نقشه حرارتی شکاف‌های ادراکی میان گروه‌های ذی‌نفع",
    subtitle = "ستاره نشان‌دهنده معناداری پس از اصلاح بونفرونی است؛ عدد داخل خانه اندازه اثر را نشان می‌دهد",
    x = "مقایسه گروه‌ها",
    y = "تعارض"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1
    ),
    panel.grid = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 9)
  )

print(p_heatmap)

# ذخیره تصویر با وضوح مناسب برای مقاله
ggsave(
  filename = "Perception_Gap_Heatmap.png",
  plot = p_heatmap,
  width = 12,
  height = 8,
  dpi = 300,
  bg = "white"
)
________________________________________


dev.new()
print(p_heatmap)
________________________________________

getwd()

file.exists("Perception_Gap_Heatmap.png")

normalizePath("Perception_Gap_Heatmap.png")
________________________________________
png(
  filename = file.path(getwd(), "Perception_Gap_Heatmap.png"),
  width = 3600,
  height = 2400,
  res = 300
)

print(p_heatmap)

dev.off()
________________________________________

shell.exec(normalizePath("Perception_Gap_Heatmap.png"))
________________________________________

getwd()
________________________________________

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
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(
      angle = 40, hjust = 1, vjust = 1
    ),
    axis.text.y = element_text(size = 8.5),
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
________________________________________
names(results_sorted)
________________________________________
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
________________________________________


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
________________________________________

scale_y_discrete(
  labels = conflict_labels,
  drop = FALSE
) +
  ________________________________________

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
________________________________________

getwd()
file.exists("Perception_Gap_Heatmap.png")
file.exists("Perception_Gap_Heatmap.pdf")
________________________________________
shell.exec(
  normalizePath(
    file.path(getwd(), "Perception_Gap_Heatmap.png")
  )
)
________________________________________


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
heatmap_data <- results_sorted %>%
  mutate(
    Group1_English = unname(group_labels[Group1_Name]),
    Group2_English = unname(group_labels[Group2_Name]),
    
    Pair = case_when(
      Group1_English == "Local residents" &
        Group2_English == "Government experts" |
        Group2_English == "Local residents" &
        Group1_English == "Government experts" ~
        pair_order[1],
      
      Group1_English == "Local residents" &
        Group2_English == "NGO members" |
        Group2_English == "Local residents" &
        Group1_English == "NGO members" ~
        pair_order[2],
      
      Group1_English == "Local residents" &
        Group2_English == "Academics" |
        Group2_English == "Local residents" &
        Group1_English == "Academics" ~
        pair_order[3],
      
      Group1_English == "Government experts" &
        Group2_English == "NGO members" |
        Group2_English == "Government experts" &
        Group1_English == "NGO members" ~
        pair_order[4],
      
      Group1_English == "Government experts" &
        Group2_English == "Academics" |
        Group2_English == "Government experts" &
        Group1_English == "Academics" ~
        pair_order[5],
      
      Group1_English == "NGO members" &
        Group2_English == "Academics" |
        Group2_English == "NGO members" &
        Group1_English == "Academics" ~
        pair_order[6],
      
      TRUE ~ NA_character_
    ),
    
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

# بررسی نام گروه‌ها و مقایسه‌های شناسایی‌نشده
if (any(is.na(heatmap_data$Pair))) {
  print(
    unique(
      results_sorted[
        is.na(
          match(
            paste(results_sorted$Group1_Name,
                  results_sorted$Group2_Name),
            paste(results_sorted$Group1_Name,
                  results_sorted$Group2_Name)
          )
        ),
        c("Group1_Name", "Group2_Name")
      ]
    )
  )
  
  if (any(is.na(heatmap_data$Group1_English)) ||
      any(is.na(heatmap_data$Group2_English))) {
    stop(
      "نام یکی از گروه‌ها با برچسب‌های تعریف‌شده مطابقت ندارد. ",
      "مقادیر Group1_Name و Group2_Name را بررسی کنید."
    )
  }
  
  stop(
    "برخی مقایسه‌های زوجی شناسایی نشدند. ",
    "نام گروه‌ها را بررسی کنید."
  )
}

# رسم نقشه حرارتی
p_heatmap <- ggplot(
  heatmap_data,
  aes(x = Pair, y = Conflict, fill = Plot_Effect)
) +
  geom_tile(
    color = "white",
    linewidth = 0.35
  ) +
  geom_text(
    aes(label = Cell_Label),
    family = "Times New Roman",
    size = 4.2,
    color = "black"
  ) +
  scale_x_discrete(
    limits = pair_order,
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
    base_size = 11
  ) +
  theme(
    plot.title = element_text(
      family = "Times New Roman",
      face = "bold",
      size = 15,
      color = "black",
      hjust = 0.5
    ),
    axis.title = element_text(
      family = "Times New Roman",
      size = 11,
      color = "black"
    ),
    axis.text.x = element_text(
      family = "Times New Roman",
      size = 10,
      color = "black",
      angle = 25,
      hjust = 1,
      vjust = 1
    ),
    axis.text.y = element_text(
      family = "Times New Roman",
      size = 10,
      color = "black"
    ),
    legend.title = element_text(
      family = "Times New Roman",
      size = 11,
      color = "black"
    ),
    legend.text = element_text(
      family = "Times New Roman",
      size = 10,
      color = "black"
    ),
    panel.grid = element_blank(),
    legend.position = "right",
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    plot.margin = margin(10, 15, 10, 10)
  )

# نمایش نقشه
print(p_heatmap)

# ذخیره در قالب PNG با وضوح مناسب مقاله
output_png <- file.path(
  getwd(),
  "Stakeholder_Perception_Gap_Heatmap.png"
)

ggsave(
  filename = output_png,
  plot = p_heatmap,
  width = 15,
  height = 8.5,
  units = "in",
  dpi = 300,
  bg = "white"
)

# نمایش مسیر و بررسی ذخیره
print(output_png)
print(file.exists(output_png))
________________________________________
warnings()
________________________________________

shell.exec(
  "C:/Users/2024/Documents/Stakeholder_Perception_Gap_Heatmap.png"
)
________________________________________
install.packages(c("showtext", "sysfonts", "ragg"))	
________________________________________

library(ggplot2)
library(showtext)
library(sysfonts)
library(ragg)

# مسیر قلم Times New Roman در ویندوز
font_regular <- "C:/Windows/Fonts/times.ttf"
font_bold <- "C:/Windows/Fonts/timesbd.ttf"

if (!file.exists(font_regular) || !file.exists(font_bold)) {
  stop(
    "فایل‌های قلم Times New Roman در مسیر پیش‌فرض ویندوز پیدا نشدند. ",
    "مسیر نصب قلم را بررسی کنید."
  )
}

# ثبت قلم برای خروجی
sysfonts::font_add(
  family = "Times New Roman",
  regular = font_regular,
  bold = font_bold
)

showtext::showtext_auto(enable = TRUE)

# برچسب‌های مقایسه زوجی با شکست خط مشخص
pair_labels <- c(
  "Local residents\nvs government\nexperts",
  "Local residents\nvs NGO\nmembers",
  "Local residents\nvs academics",
  "Government\nexperts vs NGO\nmembers",
  "Government experts\nvs academics",
  "NGO members\nvs academics"
)

# اعمال تنظیمات خوانایی بر نقشه موجود
p_publication <- p_heatmap +
  scale_x_discrete(
    breaks = pair_order,
    labels = pair_labels,
    limits = pair_order,
    drop = FALSE
  ) +
  theme_minimal(
    base_family = "Times New Roman",
    base_size = 10
  ) +
  theme(
    text = element_text(
      family = "Times New Roman",
      color = "black"
    ),
    
    plot.title = element_text(
      family = "Times New Roman",
      face = "bold",
      size = 13,
      color = "black",
      hjust = 0.5,
      margin = margin(b = 12)
    ),
    
    axis.title.x = element_text(
      family = "Times New Roman",
      size = 10,
      color = "black",
      margin = margin(t = 12)
    ),
    
    axis.title.y = element_text(
      family = "Times New Roman",
      size = 10,
      color = "black",
      margin = margin(r = 8)
    ),
    
    axis.text.x = element_text(
      family = "Times New Roman",
      size = 9,
      color = "black",
      angle = 30,
      hjust = 1,
      vjust = 1,
      lineheight = 1.05
    ),
    
    axis.text.y = element_text(
      family = "Times New Roman",
      size = 9,
      color = "black"
    ),
    
    legend.title = element_text(
      family = "Times New Roman",
      size = 10,
      color = "black"
    ),
    
    legend.text = element_text(
      family = "Times New Roman",
      size = 9,
      color = "black"
    ),
    
    panel.grid = element_blank(),
    legend.position = "right",
    plot.margin = margin(12, 18, 18, 12)
  )

# پیش‌نمایش شکل
print(p_publication)

# مسیر ذخیره
output_dir <- "C:/Users/2024/Documents"

# خروجی PDF
ggsave(
  filename = file.path(
    output_dir,
    "Stakeholder_Perception_Gap_Heatmap_Readable.pdf"
  ),
  plot = p_publication,
  device = grDevices::pdf,
  width = 18,
  height = 12,
  units = "cm",
  bg = "white"
)

# خروجی TIFF با وضوح 600 dpi
ggsave(
  filename = file.path(
    output_dir,
    "Stakeholder_Perception_Gap_Heatmap_Readable.tiff"
  ),
  plot = p_publication,
  device = ragg::agg_tiff,
  width = 18,
  height = 12,
  units = "cm",
  dpi = 600,
  compression = "lzw",
  background = "white"
)

# خروجی PNG با وضوح 300 dpi
ggsave(
  filename = file.path(
    output_dir,
    "Stakeholder_Perception_Gap_Heatmap_Readable.png"
  ),
  plot = p_publication,
  device = ragg::agg_png,
  width = 2400,
  height = 1600,
  units = "px",
  dpi = 300,
  background = "white"
)

# تأیید وجود فایل‌ها
file.exists(file.path(
  output_dir,
  "Stakeholder_Perception_Gap_Heatmap_Readable.pdf"
))

file.exists(file.path(
  output_dir,
  "Stakeholder_Perception_Gap_Heatmap_Readable.tiff"
))

file.exists(file.path(
  output_dir,
  "Stakeholder_Perception_Gap_Heatmap_Readable.png"
))
________________________________________

# ============================================================
# Publication-ready stakeholder perception-gap heatmap
# Source: Supplementary_S2_Final_96_Pairwise_Comparisons.xlsx
# Statistics and Bonferroni decisions are read from source data.
# ============================================================

# 1. Install missing packages
packages <- c("readxl", "dplyr", "ggplot2", "viridis", "tibble")

missing_packages <- packages[
  !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(readxl)
library(dplyr)
library(ggplot2)
library(viridis)
library(tibble)

# 2. Select the original Excel file
file_path <- file.choose()

raw <- read_excel(
  path = file_path,
  sheet = "96 مقایسه زوجی"
)

# 3. Check that the required columns exist
required_cols <- c(
  "کد تعارض",
  "گروه 1",
  "گروه 2",
  "اندازه اثر r",
  "نتیجه بونفرونی (α=0.0083)"
)

missing_cols <- setdiff(required_cols, names(raw))

if (length(missing_cols) > 0) {
  stop(
    "Required columns not found: ",
    paste(missing_cols, collapse = ", ")
  )
}

# 4. Define the exact intended comparison order
# L = Local residents
# E = Government experts
# N = NGO members
# A = Academics / researchers

pair_levels <- c("L-E", "L-N", "L-A", "E-N", "E-A", "N-A")

pair_labels <- c(
  "Local residents vs.\nGovernment experts",
  "Local residents vs.\nNGO members",
  "Local residents vs.\nAcademics",
  "Government experts vs.\nNGO members",
  "Government experts vs.\nAcademics",
  "NGO members vs.\nAcademics"
)

# 5. Prepare plotting data without recalculating statistics
plot_data <- raw %>%
  mutate(
    Group1 = trimws(as.character(`گروه 1`)),
    Group2 = trimws(as.character(`گروه 2`)),
    Conflict = as.character(`کد تعارض`),
    Effect_r = as.numeric(`اندازه اثر r`),
    Bonferroni_result = trimws(
      as.character(`نتیجه بونفرونی (α=0.0083)`)
    ),
    Pair = paste0(Group1, "-", Group2),
    Significant = Bonferroni_result == "معنی‌دار"
  )

# Validate pair codes and source table
if (!all(plot_data$Pair %in% pair_levels)) {
  stop(
    "Unexpected group-pair codes found. ",
    "Inspect the values in columns 'گروه 1' and 'گروه 2'."
  )
}

if (nrow(plot_data) != 96) {
  warning(
    "The source sheet has ", nrow(plot_data),
    " rows rather than the expected 96. Check the input file."
  )
}

# Every conflict should have six pairwise comparisons
pair_counts <- plot_data %>%
  count(Conflict, name = "n_pairs")

if (any(pair_counts$n_pairs != 6)) {
  warning("Some conflicts do not have exactly six pairwise comparisons.")
}

# Explicit factor order: comparisons and conflicts
conflict_levels <- paste0("C", 1:16)

plot_data <- plot_data %>%
  mutate(
    Pair = factor(Pair, levels = pair_levels),
    Conflict = factor(Conflict, levels = rev(conflict_levels)),
    # Only significant comparisons receive a coloured tile.
    Effect_significant = ifelse(Significant, Effect_r, NA_real_),
    # Preserve the original significance decisions.
    Cell_label = ifelse(
      Significant,
      paste0(sprintf("%.3f", Effect_r), "*"),
      ""
    )
  )

# 6. Create the heatmap
p_heatmap <- ggplot(
  plot_data,
  aes(x = Pair, y = Conflict, fill = Effect_significant)
) +
  geom_tile(
    colour = "white",
    linewidth = 0.55,
    width = 0.98,
    height = 0.96
  ) +
  geom_text(
    aes(label = Cell_label),
    family = "serif",
    size = 2.8,
    colour = "black",
    na.rm = TRUE
  ) +
  scale_x_discrete(
    limits = pair_levels,
    labels = setNames(pair_labels, pair_levels),
    drop = FALSE,
    expand = expansion(add = 0.02)
  ) +
  scale_y_discrete(
    limits = rev(conflict_levels),
    drop = FALSE,
    expand = expansion(add = 0.02)
  ) +
  scale_fill_viridis_c(
    option = "D",
    direction = 1,
    na.value = "white",
    name = "Effect size (r)",
    limits = range(plot_data$Effect_r[plot_data$Significant], na.rm = TRUE),
    breaks = scales::pretty_breaks(n = 5)
  ) +
  labs(
    title = "Stakeholder perception-gap effect sizes",
    subtitle = "Significant pairwise comparisons after Bonferroni correction",
    x = NULL,
    y = "Conflict"
  ) +
  theme_minimal(base_size = 10, base_family = "serif") +
  theme(
    plot.title = element_text(
      size = 13, face = "bold", colour = "black",
      hjust = 0, margin = margin(b = 5)
    ),
    plot.subtitle = element_text(
      size = 9, colour = "black",
      margin = margin(b = 12)
    ),
    axis.text.x = element_text(
      size = 8.5, colour = "black",
      angle = 25, hjust = 1, vjust = 1,
      lineheight = 0.95
    ),
    axis.text.y = element_text(
      size = 8.5, colour = "black"
    ),
    axis.title.y = element_text(
      size = 10, colour = "black",
      margin = margin(r = 8)
    ),
    legend.title = element_text(size = 9, colour = "black"),
    legend.text = element_text(size = 8.5, colour = "black"),
    legend.key.height = grid::unit(0.9, "cm"),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(12, 16, 12, 12)
  )

# Show the plot in RStudio
print(p_heatmap)

# 7. Save a true vector PDF
# cairo_pdf keeps the plot geometry vector-based.
# Text may be embedded or represented as glyph outlines,
# depending on the fonts and system configuration.
ggsave(
  filename = "Stakeholder_Perception_Gap_Heatmap_Vector.pdf",
  plot = p_heatmap,
  device = grDevices::cairo_pdf,
  width = 22,
  height = 14,
  units = "cm",
  bg = "white",
  limitsize = FALSE
)

# 8. Save a high-resolution TIFF for journal submission
ggsave(
  filename = "Stakeholder_Perception_Gap_Heatmap_600dpi.tiff",
  plot = p_heatmap,
  device = "tiff",
  width = 22,
  height = 14,
  units = "cm",
  dpi = 600,
  compression = "lzw",
  bg = "white",
  limitsize = FALSE
)

# 9. Export a transparent audit table of plotted values/decisions
write.csv(
  plot_data %>%
    mutate(
      Pair = as.character(Pair),
      Conflict = as.character(Conflict)
    ) %>%
    select(
      Conflict, Pair, Effect_r, Bonferroni_result, Significant
    ),
  file = "Heatmap_Data_Audit.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

message("Finished. Files saved in: ", getwd())
message("PDF: Stakeholder_Perception_Gap_Heatmap_Vector.pdf")
message("TIFF: Stakeholder_Perception_Gap_Heatmap_600dpi.tiff")
message("Audit: Heatmap_Data_Audit.csv")
________________________________________
getwd()
________________________________________
shell.exec(getwd())
________________________________________

# نمایش مسیر فعلی
getwd()

# جست‌وجوی فایل‌های خروجی در مسیر فعلی
list.files(
  getwd(),
  pattern = "Stakeholder_Perception|Heatmap_Data_Audit",
  recursive = TRUE,
  full.names = TRUE
)

# بررسی اینکه فایل‌های مورد انتظار وجود دارند یا نه
file.exists("Stakeholder_Perception_Gap_Heatmap_Vector.pdf")
file.exists("Stakeholder_Perception_Gap_Heatmap_600dpi.tiff")
file.exists("Heatmap_Data_Audit.csv")
________________________________________
output_dir <- file.path(
  path.expand("~"),
  "Desktop",
  "Heatmap_Outputs"
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

output_dir
________________________________________getwd()
________________________________________output_dir <- "C:/Users/2024/Documents/Desktop/Heatmap_Outputs"

# بررسی وجود پوشه
dir.exists(output_dir)

# نمایش فایل‌های داخل پوشه
list.files(output_dir, full.names = TRUE)

# جست‌وجوی فایل‌های خروجی در Documents
list.files(
  "C:/Users/2024/Documents",
  pattern = "Stakeholder_Perception|Heatmap_Data_Audit",
  recursive = TRUE,
  full.names = TRUE
)
________________________________________shell.exec(output_dir)	
________________________________________
shell.exec("C:/Users/2024/Documents")	________________________________________
