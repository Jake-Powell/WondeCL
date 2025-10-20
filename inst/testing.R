# Get fake data (note the column names are such that we can use default values)
data = WondePull::class_list_example

# Add student to a class within a school.
df_new = data |> add_student_to_school_class(first_name = 'Jimmy',
                                             last_name = 'Powell',
                                             DoB = '1910-05-07',
                                             URN = '123456',
                                             class_name = 'French')

# update the class of a particular student
df_new |> update_student_class(URN = '123456',
                               first_name = 'Jimmy',
                               last_name = 'Powell',
                               class_name = 'French',
                               new_class_name = 'Fernleaf')

# update the class of a particular student
df_new |> update_student_class(URN = '123456',
                               first_name = 'Jimmy',
                               last_name = 'Powell',
                               class_name = 'French',
                               new_class_name = 'Fernleaf')

# Change the class name 'French' in school, URN = 123456, to 'English'.
df_new |> update_class_name(URN = '123456',
                            class_name = 'French',
                            new_name = 'English')

# Change the school name 'Lavandula Primary School', URN = 123456, to
#  'Lamiaceae Lavandula Primary School'.
df_new |> update_school_name(URN = '123456',
                             new_name = 'Lamiaceae Lavandula Primary School')

# Change the teacher in 'French' class to 'Little Lottie'.
df_new |> update_class_teacher(URN = '123456',
                               class_name = 'French',
                               first_name = 'Little',
                               last_name = 'Lottie')

# Remove 'French' class from class list.
df_new |> remove_class(URN = '123456',
                       class_name = 'French')

# Remove Jimmy Powell from 'French' class, for this to word each student requires a unique id.
df_new$StudentId = 1:nrow(df_new)
student_rm = df_new$StudentId[df_new$`Pupil Last Name` == 'Powell']
df_new |> remove_student_from_class(URN = '123456',
                                    class_name = 'French',
                                    student_id = student_rm)


## Create some blanked schools.
A = c('Narcissus Primary School', 'Tulipa Primary School')
B = c('321123', '567765')
C = c('Pseudonarcissus -> 10', 'Albanica -> 30, Persica -> 29')
D = c('Carl---Linnaeus','Kit---Tan, Robert---Sweet ')
sch_info = data.frame(SchoolName = A,
                      URN = B,
                      ClassInfo = C,
                      TeacherInfo = D)
sch_info = data.frame(SchoolName = A,
                      URN = B,
                      NoStudents = c(25, 60))
out = create_school_blanks(sch_info)

