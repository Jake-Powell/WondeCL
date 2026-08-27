Headers = c('EST_ID',	'EstablishmentName',	'URN',	'Category',	'SchoolType',	'Class_ID',	'Class Name',	'Teacher_ID',	'Main Teacher First Name',	'Main Teacher Last Name',	'OME_ID',	'Print ID',	'UPN',	'Pupil First Name',	'Pupil Last Name',	'DoB')

# for pupil first name we use -> https://github.com/smashew/NameDatabases/blob/master/NamesDatabases/first%20names/us.txt
# last name -> https://gist.github.com/lasagnaphil/7667eaeddb6ed0c565f0cb653d756942
FN = c('Melina', 'Gary', 'Demarcus', 'Marcelo', 'Aliza', 'Germaine', 'Jonell', 'Marcos', 'Chantelle', 'Chi', 'Gertrude', 'Cherry', 'Kenton', 'Bula', 'Elisabeth', 'Kandra', 'Aleen', 'Shenita', 'Laurinda', 'Lissa', 'Kera', 'Janett', 'Jana', 'Sandy', 'Brain', 'Olevia', 'Dannette', 'Tisha', 'Caitlin', 'Novella', 'Clementina', 'Pandora', 'Janey', 'Cherryl', 'Erline', 'Lawana', 'Eliana', 'Rich', 'Shantelle', 'Cletus', 'Pauletta', 'Ollie', 'Joline', 'Rudy', 'Chantell', 'Kaye', 'Gilda', 'Russell', 'Ardath', 'Thea', 'Riley', 'Candelaria', 'Emeline', 'Teisha', 'Emeline', 'Shawnee', 'Elidia', 'Denita', 'Maira', 'Grayce')
LN = c('Apple', 'Apricot', 'Avocado', 'Banana', 'Bilberry', 'Blackberry', 'Blackcurrant', 'Blueberry', 'Boysenberry', 'Currant', 'Cherry', 'Cherimoya', 'Chico fruit', 'Cloudberry', 'Coconut', 'Cranberry', 'Cucumber', 'Custard apple', 'Damson', 'Date', 'Dragonfruit', 'Durian', 'Elderberry', 'Feijoa', 'Fig', 'Goji berry', 'Gooseberry', 'Grape', 'Raisin', 'Grapefruit', 'Guava', 'Honeyberry', 'Huckleberry', 'Jabuticaba', 'Jackfruit', 'Jambul', 'Jujube', 'Juniper berry', 'Kiwano', 'Kiwifruit', 'Kumquat', 'Lemon', 'Lime', 'Loquat', 'Longan', 'Lychee', 'Mango', 'Mangosteen', 'Marionberry', 'Melon', 'Cantaloupe', 'Honeydew', 'Watermelon', 'Miracle fruit', 'Mulberry', 'Nectarine', 'Nance', 'Olive', 'Orange', 'Blood orange', 'Clementine', 'Mandarine', 'Tangerine', 'Papaya', 'Passionfruit', 'Peach', 'Pear', 'Persimmon', 'Physalis', 'Plantain', 'Plum', 'Prune', 'Pineapple', 'Plumcot', 'Pomegranate', 'Pomelo', 'Purple mangosteen', 'Quince', 'Raspberry', 'Salmonberry', 'Rambutan', 'Redcurrant', 'Salal berry', 'Salak', 'Satsuma', 'Soursop', 'Star fruit', 'Solanum quitoense', 'Strawberry', 'Tamarillo', 'Tamarind', 'Ugli fruit', 'Yuzu')

## Student master list
set.seed(12)
A = c(rep('A',30), rep('B', 30))
B = c(rep('Lavandula Primary School', 30), rep('Digitalis Purpurea Primary School', 30))
C = c(rep('123456', 30), rep('654321', 30))
D = rep('Primary', 60)
E = rep('P', 60)
G = c(rep('449073-1', 15), rep('449089-1', 15), rep('802077-1', 30))
H = c(rep('Fernleaf', 15), rep('French', 15), rep('Foxglove', 30))
I = c(rep('Cult-1992', 15), rep('Cult-1996', 15), rep('Cult-1990s', 30))
J = c(rep('Miss', 15), rep('Linda', 15), rep('Elsie', 30))
K = c(rep('Katherine', 15), rep('Ligon', 15), rep('Kelsey', 30))
L = 1:60
M = 1:60
N = lapply(1:60, function(x)sample(c(LETTERS, 0:9), 13) |> paste0(collapse='') ) |> unlist()
O = FN
P = LN |> sample(60)
Q = paste0('1910-',
           sample(c(paste0(0,1:9), 11,12),size = 60, replace = T),
           '-',
           sample(c(paste0(0,1:9), 11:28),size = 60, replace = T))


class_list = data.frame(A,B,C,D,E,G,H,I,J, K,L,M,N,O,P,Q)
names(class_list) = Headers
class_list_example = class_list
usethis::use_data(class_list_example, overwrite = T)

# Fake Teacher Master List corresponding to class_list_example

teacher_master_list_example <- data.frame(
  OME_ID = c(
    "Cult-1992",
    "Cult-1996",
    "Cult-1990s",
    "Cult-1983",
    "Cult-1989",
    "Cult-1931"
  ),
  
  ExternalReference = c(
    "EXT-T001",
    "EXT-T002",
    "EXT-T003",
    "EXT-T004",
    "EXT-T005",
    "EXT-T006"
  ),
  
  FirstName = c(
    "Miss",
    "Linda",
    "Elsie",
    "Irene",
    "Sharon",
    "Summer"
  ),
  
  LastName = c(
    "Katherine",
    "Ligon",
    "Kelsey",
    "Doyle",
    "Roberts",
    "King"
  ),
  
  EST_ID = c(
    "A",
    "A",
    "B",
    "A",
    "A",
    "B"
  ),
  
  EstablishmentType = rep("Primary", 6),
  
  Class_ID = c(
    "449073-1",                 # Fernleaf
    "449089-1",                 # French
    "802077-1",                 # Foxglove
    "449073-1",                 # additional Fernleaf teacher
    "449073-1, 449089-1",      # teaches both Lavandula classes
    "802077-1"                  # additional Foxglove teacher
  ),
  
  LastUpdated = as.Date(c(
    "2026-08-01",
    "2026-08-01",
    "2026-08-01",
    "2026-08-05",
    "2026-08-05",
    "2026-08-07"
  )),
  
  Notes = c(
    NA,
    NA,
    NA,
    "Example additional teacher",
    "Example teacher associated with two classes",
    "Example additional teacher"
  ),
  
  FNs = c(
    "Miss",
    "Linda",
    "Elsie",
    "Irene",
    "Sharon",
    "Summer"
  ) |> tolower(),
  
  LNs = c(
    "Katherine",
    "Ligon",
    "Kelsey",
    "Doyle",
    "Roberts",
    "King"
  ) |> tolower(),
  
  Wonde_id = c(
    "WONDE-T001",
    "WONDE-T002",
    "WONDE-T003",
    "WONDE-T004",
    "WONDE-T005",
    "WONDE-T006"
  ),
  
  Wonde_mis_id = c(
    "MIS-T001",
    "MIS-T002",
    "MIS-T003",
    "MIS-T004",
    "MIS-T005",
    "MIS-T006"
  )
)

teacher_master_list_example
usethis::use_data(teacher_master_list_example, overwrite = T)
