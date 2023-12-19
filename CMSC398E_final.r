library(RMySQL)
library(pacman)
library(ggplot2)
library(GGally)
library(caret)
p_load(tidyverse,rpart,tidymodels)

# Hypothesis
# Using the hotel booking data set from kaggle, the problem I identify is why are people cancelling their booking?
# There are many useful bits of information that this data set provides, such as the average price of the booking,
# the number of children the reserver(s) have, the number of week/weekend days booked, and booking status.
# Using these pieces of information, I believe we can discover if they play a significant factor in cancelling the booking.
# Thus, my hypothesis is that booking status is dependent on the average price of the booking,
# the number of children the reserver(s) have, and the number of week/weekend days booked.
# Null Hypothesis: Booking status is NOT dependent on the average price of the booking,
# the number of children the reserver(s) have, and the number of week/weekend days booked.
##################################################################################################################################
#                                         Setting a MySQLWorkbench Connection Password
##################################################################################################################################

# Select a connection
# Go to "administration" on the top of the left side panel
# Under administration, select "users and privileges"
# Select root as user and set password
# Close connection
# Enter password and use connection as usual


##################################################################################################################################
#                                                   Connecting MYSQL to R
##################################################################################################################################

# For MySQLWorkbench dbname will be the name of your schema NOT your database connection
# DO NOT CHANGE host, port, or user unless you intentionally changed those variables when creating your db
# If you set a password input your password, otherwise leave delete the parameter
mysqlconnection = dbConnect(RMySQL::MySQL(),
                            dbname='final',
                            host='localhost',
                            port=3306,
                            user='root',
                            password='CMSC398eFinal')
# Shows all tables in schema
dbListTables(mysqlconnection)
# Allows you to pull a query using the sql connection
# If you use double quotes in your query, convert them to single quotes to prevent errors

# For my query, I filled in the missing number of children values with the average 
# of the column. Furthermore, I simply renamed the output columns to not have spaces,
# and I used the replace function to change 'Not_Canceled' simply to 'Not' for convenience. 
# Lastly, I replace instance of 'Not' as 0 and 'Canceled' as 1 and partitioned by the average price
# per booking status.
query = "with avg_children as (
	select avg(`number of children`) as avg_num
    from final.booking
), t1 as (
	select 
		`number of weekend nights` as num_weekend_nights,
		`number of week nights` as num_week_nights,
		`average price` as avg_price,
		case
			when `number of children` is null then (select avg_num from avg_children)
			else `number of children`
		end as num_children,
		replace(`booking status`, 'Not_Canceled', 'Not') as booking_status
	from final.booking
)
select
    num_weekend_nights,
    num_week_nights,
    avg_price,
    num_children,
    case 
		when booking_status = 'Not' then 0
        else 1
	  end as booking_status,
    avg(avg_price) over (partition by booking_status) as avg_price_per_status
from
    t1;"

result = dbSendQuery(mysqlconnection, query) 
# Stores resulting table as dataframe
df = fetch(result)
# Histogram showing the frequency of the number of children the reservers had. 
hist(df$num_children)
# Histogram showing the frequency of the average prices reservers would spend on their booking.
hist(df$avg_price)
# Scatterploy showing the relationship between number of week nights that was reserved and the average price.
s_plot <- data.frame(num_week_nights = df$num_week_nights, avg_price = df$avg_price)
scatterplot <- ggplot(s_plot, aes(x = num_week_nights, y = avg_price)) + geom_point()
scatterplot
# Linear Regression Model
# X = booking_status
# Y = All other columns.
model <- lm(booking_status ~ num_weekend_nights + num_week_nights + avg_price + num_children, data = df)
summary(model)
# Hypothesis: booking status is dependent on the average price of the booking,
# the number of children the reserver(s) have, and the number of week/weekend days booked.
# Null Hypothesis: Booking status is NOT dependent on the average price of the booking,
# the number of children the reserver(s) have, and the number of week/weekend days booked.
# RESULT:
# According to the statistics from the model, the p-value was < 2.2e-16.
# Therefore, we can confidently reject the null hypothesis for any alpha significance level
# and conclude that booking status DOES depend on the average price of the booking,
# the number of children the reserver(s) have, and the number of week/weekend days booked.
