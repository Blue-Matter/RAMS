
create_dummy_users <- function() {
  user_df <- data.frame(idUSER = c("user1", "user2"),
                        PASSWORD = sapply(c("pass1", "pass2"), sodium::password_store),
                        PERMISSIONS = c("admin", "facilitator"),
                        FIRST_NAME = c('John', 'Bob'),
                        LAST_NAME = c('Dorey', 'Down'),
                        EMAIL = c('j.dory@email.com', 'b.down@email.com'),
                        DATE_CREATED = Sys.time()
             )
  rownames(user_df) <- NULL
  write.csv(user_df, 'data-raw/USERS.csv', row.names = FALSE)

}

create_dummy_users()

