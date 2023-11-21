# 1. Відтворіть наступний графік, який складено за даними датасету women з базової
# інсталяції R.

data(women)
head(women)
plot(women$height, women$weight, xlab = "Height", ylab = "Weight", 
     col="mediumpurple1",main = "American Women: Weight vs Height", pch = 19)

# 2. В R є вбудований датасет Nile. Цей датасет містить річний потік води в річці 
# Ніл з 1871 до 1971 року. З використанням функції plot та її параметрів:
#   1)зобразити графік залежності величини річкового потоку за роками;
#   2)додатизаголовокдля графіка: «Nile River Annual Flow», підпис осі 
# x: «Year» і підпис осі y: «Flow»;
#   3)обчислити середнє значення річкового потоку додати на графік горизонтальну 
# лінію, яка відображає середній потік за всі рокиі підпис «Average  Flow» з 
# підрахованим середнім потоком.

data(Nile)
Nile

# графік залежності величини річкового потоку за роками
plot(Nile, xlab = "Year", ylab = "Flow", 
     main = "Nile River Annual Flow", col = "blue")

# Обчислення середнього значення річкового потоку
avg_flow <- mean(Nile)

# Додавання горизонтальної лінії для відображення середнього потоку
abline(h = avg_flow, col = "red", lty = 2)
legend("topright", legend = "Average Flow", col = "red", lty = 2, cex = 0.8)


# 3.В датасеті cars наведено швидкість і гальмівну відстань. Зобразіть дані на 
# точковому графіку і проведіть аналіз отриманих результатів. 

data(cars)

plot(cars$speed, cars$dist, xlab = "Speed", ylab = "Stopping Distance", 
     main = "Speed vs Stopping Distance", col = "mediumpurple1", pch = 19)

# 4.Використайте датасет rivers (RBase). Створіть графік, який містить зображення 
# річок щодо їх індексу в даних. Додайте підпис осі Y: «Length  in  miles». 
# Додайте заголовок червоним кольором у два рядки: «Length  of  Major  N. 
# American  Rivers». Точки зобразіть зеленим кольором.

data(rivers)

# Створення графіку з точковими даними, зеленим кольором і підписами осей
plot(rivers, col = "mediumpurple1", pch = 19,
     xlab = "River Index", ylab = "Length in miles")

# Додавання червоного заголовка у два рядки
title(main = expression(paste("Length of Major N. American Rivers",
                              collapse="\n")), col.main = "red")


# Стовпчикові діаграми та гістограми
 
# 5.Створіть вектор з таких чисел: 60 85 72 59 37 75 93 7 98 63 41 90 5 17 97
# і побудуйте для такого вектору гістограму та стовпчикову діаграму, розмістивши
# їх в один ряд. В чому буде полягати відмінність?

vector <- c(60, 85, 72, 59, 37, 75, 93, 7, 98, 63, 41, 90, 5, 17, 97)
colors <- rainbow(length(vector))
par(mfrow = c(1, 2))
hist(vector, main = "Гістограма", xlab = "Значення", ylab = "Частота", col = colors)

barplot(vector, main = "Стовпчикова діаграма", xlab = "Індекс", ylab = "Значення", col = colors) 


# 6. З допомогою команди rnorm(100) згенеруйте 100 випадкових чисел з нормальним 
# розподілом.  Створіть  дві  гістограми  з  двома  різними  наборами  чисел  
# по 100 елементів  з  нормального  розподілу,  використавши  команду  наведену  
# вище.  Чи гістограми будуть однаковими? 

set.seed(123)  # Забезпечення відтворюваності результатів
values_1 <- rnorm(n = 100, mean = 10, sd = 5)
values_2 <- rnorm(n = 100, mean = 10, sd = 5)

par(mfrow = c(1, 2))

hist(values_1, main = "Гістограма 1", xlab = "Значення", ylab = "Частота", col = "mediumpurple1")
hist(values_2, main = "Гістограма 2", xlab = "Значення", ylab = "Частота", col = "darkolivegreen1")



# 7. Завантажте дані  з  файлу firtree.csv. Побудуйте  стовпчикову  діаграму  
# розподілу кількості хвойних дерев у числовому та відсотковому форматі з 
# різними відтінками зеленого кольору.

tree <- read.csv("firtree.csv")
tree
par(mfrow = c(1, 2))

counts <- table(tree$ftype)
barplot(counts, main = "Розподіл кіл-ті дерев",
        xlab = "Тип хвойного дерева", ylab = "Кількість",
        col = c("darkgreen", "green", "lightgreen"))

prop_table <- prop.table(counts) * 100
barplot(prop_table, main = "Розподіл кіл-ті дерев",
        xlab = "Тип хвойного дерева", ylab = "Відсоток",
        col = c("darkgreen", "green", "lightgreen"))


# 8. Побудуйте гістограму для висоти дерев. Додайте до гістограми 
# заголовок, змініть колір і підпишіть осі координат
par(mfrow = c(1, 1))
hist(tree$height, main = "Розподіл висоти дерев", xlab = "Висота дерева", ylab = "Кількість",
     col = "skyblue")

# Кругові діаграми

# 9. Побудуйте  кругову діаграму  розподілу  типів  хвойних  дерев.  
# Змініть кольори діаграми на власні. 

colors_2 <- c("skyblue", "lightgreen", "orange", "pink")
pie(counts, main = "Розподіл типів хвойних дерев", col = colors_2)

# 10. Додайте до кругової діаграми значення розподілу дерев у відсотках.
# Додати легенду до кругової діаграми.

pie(counts, main = "Розподіл типів хвойних дерев",
    col = colors_2, labels = paste(prop_table, "%"))

legend(x=1, y=1.2, legend = names(tree_counts), 
       cex = 0.8, fill = colors_2)
