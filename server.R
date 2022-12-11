load("genre_matrix.rda") 
load("ibcf.rda")

library(dplyr)
library(shiny)

# read in data
myurl = "https://liangfgithub.github.io/MovieData/"
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))

##################### System 1 #####################

# Get unique movie genres for the dropdown
genre_list = c("Action", "Adventure", "Animation",
               "Children's", "Comedy", "Crime",
               "Documentary", "Drama", "Fantasy",
               "Film-Noir", "Horror", "Musical",
               "Mystery", "Romance", "Sci-Fi",
               "Thriller", "War", "Western")

ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'),
                   sep = ':',
                   colClasses = c('integer', 'NULL'),
                   header = FALSE)

colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')

i = paste0('u', ratings$UserID)
j = paste0('m', ratings$MovieID)
x = ratings$Rating
tmp = data.frame(i, j, x, stringsAsFactors = T)
Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
rownames(Rmat) = levels(tmp$i)
colnames(Rmat) = levels(tmp$j)
Rmat = new('realRatingMatrix', data = Rmat)
Rmat = Rmat[1:500, ] # let's use a subset

## average rating for each movie in a specific genre
allmovies_genre = function(genre) {
  movieid_genre = genre_matrix[,genre] == 1
  movies_genre = movies[movieid_genre,]
  
  ratings <- ratings %>% 
    group_by(MovieID) %>% 
    mutate(
      avg_ratings = round(mean(Rating), dig=4)
    ) %>%
    select(-Rating) 
  
  avg_rating<- distinct(ratings, MovieID, avg_ratings)
  
  ret = movies_genre %>%
    left_join(avg_rating, by = 'MovieID') %>%
    replace(is.na(.), 0) %>% 
    mutate(avg_rating = dense_rank(desc(avg_ratings))) %>% 
    arrange(desc(avg_ratings)) %>%
    distinct()
  
  return(ret)
}

# top N rated movies based on the average rating
topN_movies_genre = function(top_n, genre) {
  full_movies_by_genre = allmovies_genre(genre)
  ret = full_movies_by_genre %>%
    arrange(desc(avg_ratings)) %>%
    mutate(avg_rank = dense_rank(desc(avg_ratings))) %>%
    head(top_n) %>%
    select('MovieID', 'Title', 'avg_rank', 'avg_ratings')
  return(ret)
}

# for system 2
get_user_ratings = function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
}

# most 30 popular movies to rate
pop = ratings %>% 
  group_by(MovieID) %>% 
  summarize(ratings_per_movie = n(), ave_ratings = mean(Rating)) %>%
  inner_join(movies, by = 'MovieID') %>%
  arrange(desc(ratings_per_movie)) %>%
  select(c("Title", "ratings_per_movie","MovieID")) %>%
  head(30)

########################## shinyServer #########################
shinyServer(function(input, output, session) {
  
    ######## system 1 ########
    df_genre <- eventReactive(input$button_Genre, {
        withBusyIndicatorServer("button_Genre", {
          transition_to_loading_state()
          value_list = reactiveValuesToList(input)
          selected_genre = value_list$genreDropdown
          top_genre_movies = topN_movies_genre(12, selected_genre) # 8 top-rated movies
          recom_genre_results <- data.table(Rank = 1:12, 
                                            MovieID = top_genre_movies$MovieID, 
                                            Title = top_genre_movies$Title)})
        })
  
    # show genre dropdown
    output$genres_dropdown <- renderUI({
      selectInput("genreDropdown", "Genre:", as.list(genre_list))
    })

    # display the recommendations
    output$results_by_genre <- renderUI({
      num_rows <- 2
      num_movies <- 6
      recom_result = df_genre()
      lapply(1:num_rows, function(i) {
        list(fluidRow(lapply(1:num_movies, function(j) {
          movie_idx = (i - 1) * num_movies + j
          movie_id = recom_result$MovieID[movie_idx]
          movie_title = recom_result$Title[movie_idx]
          rec_movie = movies[movies$MovieID == movie_id,]
          box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
              div(style = "text-align:center", a(img(src = rec_movie$image_url, height = 150))),
              div(style="text-align:center; font-size: 100%", strong(movie_title))
          )        
        }))) # columns
      }) # rows
    })
  
    #Hide ratings container
    transition_to_loading_state <- function() {
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
    }

    ######## system 2 ########
    
    # show the popular movies to be rated
    output$ratings <- renderUI({
      num_rows <- 5
      num_movies <- 6 # movies per row
      
      lapply(1:num_rows, function(i) {
        list(fluidRow(lapply(1:num_movies, function(j) {
          movie_idx2 = (i - 1) * num_movies + j
          movie_id2 = pop$MovieID[movie_idx2]
          movie_title2 = pop$Title[movie_idx2]
          chosen_movie = movies[movies$MovieID == movie_id2,]
          list(box(width = 2,
                   div(style = "text-align:center", img(src = chosen_movie$image_url, height = 150)),
                   div(style = "text-align:center", strong(movie_title2)),
                   div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[movie_id2]), label = "", dataStop = 5))))
        })))
      })
    })
    
    # Calculate recommendations when the sbumbutton is clicked
    df <- eventReactive(input$btnRating, {
      withBusyIndicatorServer("btnRating", { # showing the busy indicator
        # hide the rating container
        useShinyjs()
        jsCode <- "document.querySelector('[data-widget=collapse]').click();"
        runjs(jsCode)
        
        # get the user's rating data
        value_list <- reactiveValuesToList(input)
        user_ratings <- get_user_ratings(value_list)
        user_ratings <- as.vector(unlist(user_ratings))
        
        MID=paste0("m", pop$MovieID)
        movieIDs = colnames(Rmat)
        n.item = ncol(Rmat)  
        new.ratings = rep(NA, n.item)
        
        for (i in 1:30) {
          new.ratings[which(movieIDs == MID[i])] = user_ratings[i] 
        }
        
        new.user = matrix(new.ratings, 
                          nrow=1, ncol=n.item,
                          dimnames = list(
                            user=paste('newuser'),
                            item=movieIDs
                          ))
        new.Rmat = as(new.user, 'realRatingMatrix')
        
        recom = predict(recommender.IBCF, new.Rmat, type = 'topN', n=20)
        user_results = unlist(recom@ratings)
        user_predicted_ids = substring(as(recom, 'list')[[1]],2)
        
        result <- subset(movies, movies$MovieID %in% user_predicted_ids)
        recom_results <- data.table(Rank = 1:18, 
                                    MovieID = result$MovieID, 
                                    Title = result$Title, 
                                    Url = result$image_url,
                                    Predicted_rating = user_results)
      }) 
      
    }) # clicked on button
    
    # display the 18 recommendations
    output$results <- renderUI({
      num_rows <- 3
      num_movies <- 6
      recom_result_display <- df()
      
      lapply(1:num_rows, function(i) {
        list(fluidRow(lapply(1:num_movies, function(j) {
          movie_idx = (i - 1) * num_movies + j
          movie_id = recom_result_display$MovieID[movie_idx]
          movie_title = recom_result_display$Title[movie_idx]
          box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
              div(style = "text-align:center", 
                  a(img(src = movies$image_url[recom_result_display$MovieID[(i - 1) * num_movies + j]], height = 150))),
              div(style="text-align:center; font-size: 100%", 
                  strong(movies$Title[recom_result_display$MovieID[(i - 1) * num_movies + j]]))
          )        
        }))) # columns
      }) # rows
      
    }) # renderUI function
})
