Headers = c("School name",	"URN",	"Class Name",	"Main Teacher First Name",	"Main Teacher Last Name",	"Pupil First Name",	"Pupil Last Name",	"DoB",	"UPN")

# for pupil first name we use -> https://github.com/smashew/NameDatabases/blob/master/NamesDatabases/first%20names/us.txt
# last name -> https://gist.github.com/lasagnaphil/7667eaeddb6ed0c565f0cb653d756942
{
  FN = read.delim('/Users/jakepowell/Downloads/us.txt')
  LN = 'Apple
Apricot
Avocado
Banana
Bilberry
Blackberry
Blackcurrant
Blueberry
Boysenberry
Currant
Cherry
Cherimoya
Chico fruit
Cloudberry
Coconut
Cranberry
Cucumber
Custard apple
Damson
Date
Dragonfruit
Durian
Elderberry
Feijoa
Fig
Goji berry
Gooseberry
Grape
Raisin
Grapefruit
Guava
Honeyberry
Huckleberry
Jabuticaba
Jackfruit
Jambul
Jujube
Juniper berry
Kiwano
Kiwifruit
Kumquat
Lemon
Lime
Loquat
Longan
Lychee
Mango
Mangosteen
Marionberry
Melon
Cantaloupe
Honeydew
Watermelon
Miracle fruit
Mulberry
Nectarine
Nance
Olive
Orange
Blood orange
Clementine
Mandarine
Tangerine
Papaya
Passionfruit
Peach
Pear
Persimmon
Physalis
Plantain
Plum
Prune
Pineapple
Plumcot
Pomegranate
Pomelo
Purple mangosteen
Quince
Raspberry
Salmonberry
Rambutan
Redcurrant
Salal berry
Salak
Satsuma
Soursop
Star fruit
Solanum quitoense
Strawberry
Tamarillo
Tamarind
Ugli fruit
Yuzu' |> stringr::str_split('\n') |> unlist()

}



## Student master list
set.seed(45)
A = c(rep('Lavandula Primary School', 30), rep('Digitalis Purpurea Primary School', 30))
B = c(rep('123456', 30), rep('654321', 30))
C = c(rep('Fernleaf', 15), rep('French', 15), rep('Foxglove', 30))
D = c(rep('Miss', 15), rep('Willow', 15), rep('Elsie', 30))
E = c(rep('Katherine', 15), rep('Vale', 15), rep('Kelsey', 30))
G = sample(FN[,1], 60, replace = T)
H = sample(LN, 60, replace = T)
I = paste0('1910-',
           sample(c(paste0(0,1:9), 11,12),size = 60, replace = T),
           '-',
           sample(c(paste0(0,1:9), 11:28),size = 60, replace = T))
J = lapply(1:60, function(x)sample(c(LETTERS, 0:9), 13) |> paste0(collapse='') ) |> unlist()

class_list = data.frame(A,B,C,D,E,G,H,I,J)
names(class_list) = Headers
class_list_example = class_list
usethis::use_data(class_list_example)

## Teacher master list
A = c(rep('Lavandula Primary School', 2), rep('Digitalis Purpurea Primary School', 1))
B = c(rep('123456', 2), rep('654321', 1))
C = c(rep('Fernleaf', 1), rep('French', 1), rep('Foxglove', 1))
D = c(rep('Miss', 1), rep('Willow', 1), rep('Elsie', 1))
E = c(rep('Katherine', 1), rep('Vale', 1), rep('Kelsey', 1))
G = rep('TRUE', 3)
teacher_list = data.frame(A,B,C,D,E,G)
names(teacher_list) = c(Headers[1:5], 'IsMainTeacher')
teacher_list[4,] =

