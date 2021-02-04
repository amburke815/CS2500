;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname HW7_AdamBurke) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;Homework 7
;CS2500
;10/24/2020
;Adam Burke and Gabby Garcia

;Exercise 2

(define TS-MAX-LENGTH 280)
;A TweetString is a String of length being a Natural in the range [1,280] characters
;interp.: A string representing a tweet. This string cannot exceed a length of 280 characters and can
;has to be at least one character long
;EXAMPLES:
(define ts-trump "I will be leaving the great Walter Reed Medical Center today at 6:30 P.M.
 Feeling really good! Don’t be afraid of Covid. Don’t let it dominate your life. We have developed,
under the Trump Administration, some really great drugs & knowledge. I feel better than I did 20 years
 ago!")
(define ts-kanye "Man ... ninjas are kind of cool ... I just don't know any personally")
(define ts-me "NORMALIZE RABIES!!!!!!! GRRR!!!!!")
(define ts-280-chars "blablablablablablabla
blablablablablablabla
blablablablablablabla
blablablablablablabla
blablablablablablabla
blablablablablablabla
blablablablablablabla

blablablablablablabla
blablablablablablabla
blablablablablablabla
BlablablablablablablablablablablabTHIS IS THE 280th char: !")
(define ts-281-chars "blablablablablablabla
blablablablablablabla
blablablablablablabla
blablablablablablabla
blablablablablablabla
blablablablablablabla
blablablablablablabla

blablablablablablabla
blablablablablablabla
blablablablablablabla
BlablablablablablablablablablablabTHIS IS THE  281st char: !")
;A template is not needed for this data since it is atomic data in a restricted range. I have gotten
;points off for this in the past so I'm including one just in case. I will not call it in later
;templates because that is useless:
;ts-templ: TweetString -> ???
#;(define (ts-templ ts)
    (...ts...))

(define-struct tweet [text likes retweets])
;A Tweet is a (make-tweet TweetString Natural Natural)
;interp.: the contents of a tweet, (make-tweet t l r), i.e. a post on twitter with containing the
;message _t_, with _l_ likes, and _r_ retweets
;EXAMPLES:
(define tweet-1 (make-tweet ts-trump 594800000 8940000))
(define tweet-2 (make-tweet ts-kanye
                            234233
                            2391000))
(define tweet-3 (make-tweet ts-me 8 3))
(define failed-tweet-0-0 (make-tweet "can we normalize 5 likes?" 0 0))
(define failed-tweet-0-1 (make-tweet "can we normalize 5 likes?" 0 1))
(define failed-tweet-1-0 (make-tweet "can we normalize 5 likes?" 1 0))
(define failed-tweet (make-tweet "can we normalize 6 likes?" 1 0))
(define tweet-280-chars (make-tweet ts-280-chars 12 0))
(define tweet-281-chars (make-tweet ts-281-chars 12 0))
;tweet-templ: Tweet -> ???
#;(define (tweet-templ t)
    (... (tweet-text t)
         (tweet-likes t)
         (tweet-retweets t)))

(define-struct fbpost [text likes])
;A FBpost is a (make-fbpost String Natural)
;interp.: A post on facebook, (make-fbpost t l) has text _t_ and _l_ likes
;EXAMPLES:
;I could name these something boring like fbpost-1, fbpost-2, etc, but these seemed like more fun for
;you and I both. They're all real fb posts that grandmas who don't understand fb made and they all got
;consolidated into a political compass meme. I'll define some boring ones below too just in case
(define fbpost-left-authoritarian (make-fbpost "I do not give facebook permission to print anything
off my computer" 10))
(define fbpost-right-authoritarian (make-fbpost "I LOVE GOD, MY FAMILY, MY PRESIDENT, AND MY COUNTRY.
I HAVE HIGH BLOOD PRESSURE" 25))
(define fbpost-right-libertarian (make-fbpost "BK double cheeseburger, fries and apple pie." 5))
(define fbpost-left-libertarian (make-fbpost "I AM A B*TCH AND PROUD OF IT !!!!!!!!!!!!!" 32))
(define fbpost-1 (make-fbpost "Oh gee, I love facebook" 10))
(define fbpost-2 (make-fbpost "ooh boy, facebook am i right? aint it swell??" 3))
(define fbpost-3 (make-fbpost "golly gee jeepers! Facebook! Wowee!!!" 4))
(define failed-fbpost (make-fbpost "can we hit 5 likes?" 0))
(define fbpost-9-likes (make-fbpost "this post will get 9 likes" 9))
(define fbpost-10-likes (make-fbpost "this post will get 10 likes" 10))
;fbpost-templ: FBpost -> ???
#;(define (fbpost-templ fbp)
    (... (fbpost-text fbp)
         (fbpost-likes fbp)))

(define-struct story [text views])
;A Story is a (make-story String Natural)
;interp.: a Medium story, (make-story t v) has text _t_ and _v_ page views online
;EXAMPLES:
(define story-1 (make-story "You'll never believe this new medical secret doctors don't want you to
know!" 10000000))
(define story-2 (make-story "top 100 staircases in horror movies" 20))
(define story-3 (make-story "this is a story online" 99))
(define failed-story (make-story "top 50 ben shapiro moments that totally owned the libs!" 0))
(define story-for-ta (make-story "hi ta! can you tell that I'm worried about the outcome of the
election?" 100))
;story-templ: Story -> ???
#;(define (story-templ s)
    (... (story-text s)
         (story-views s)))

;A Post is one of:
;- Tweet
;- FBpost
;- Story
;interp.: Some form of posted social media content on the internet. Either a tweet on twitter, post
;on facebook, or story on medium
;EXAMPLES:
(define post-twitter tweet-1)
(define post-fbpost fbpost-right-libertarian)
(define post-story story-1)
;post-templ: Post -> ???
#;(define (post-templ p)
    (cond [(tweet? p) (... (tweet-templ p))]
          [(fbpost? p) (... (fbpost-templ p))]
          [(story? p) (... (story-templ p))]))


;Exercise 3

;FIX THIS TO TAKE A POST NOT A TWEET!

;sort-for->=1retweets: [List-of Post] -> [List-of Tweet]
;given a list of tweets, this function produces a list of all the tweets in that list that have at
;least one retweet
(check-expect (sort-for->=1retweets '())
              '())
(check-expect (sort-for->=1retweets (list tweet-1 tweet-2 failed-tweet)) (list tweet-1 tweet-2))
(check-expect (sort-for->=1retweets (list failed-tweet)) '())
(check-expect (sort-for->=1retweets (list tweet-1 failed-tweet failed-tweet)) (list tweet-1))
(check-expect (sort-for->=1retweets (list tweet-1 fbpost-1 story-1)) (list tweet-1))
(check-expect (sort-for->=1retweets (list fbpost-1 fbpost-2 fbpost-3 story-3 story-1)) '())
(define (sort-for->=1retweets lop)
  (filter at-least-one-retweet? (filter-for-tweets lop)))

;at-least-one-retweet?: Tweet -> Boolean
;does the given tweet have at least one retweet?
(check-expect (at-least-one-retweet? tweet-1) #t)
(check-expect (at-least-one-retweet? tweet-2) #t)
(check-expect (at-least-one-retweet? failed-tweet) #f)
(check-expect (at-least-one-retweet? (make-tweet "bla" 23 1)) #t)
(define (at-least-one-retweet? t)
  (>= (tweet-retweets t) 1))

;filter-for-tweets: [List-of Post] -> [List-of Tweet]
;extracts only Tweets from a [List-of Post] to a new list
(check-expect (filter-for-tweets '())
              '())
(check-expect (filter-for-tweets (list tweet-1 tweet-2 fbpost-1 fbpost-2 story-1 story-2))
              (list tweet-1 tweet-2))
(check-expect (filter-for-tweets (list tweet-1 tweet-2))
              (list tweet-1 tweet-2))
(check-expect (filter-for-tweets (list fbpost-1 fbpost-2 story-1 story-2))
              '())
(check-expect (filter-for-tweets (list tweet-1 fbpost-1 fbpost-2 story-1 story-2))
              (list tweet-1))
(define (filter-for-tweets lop)
  (filter tweet? lop))
              

;Exercise 4

;sort-for-failed-posts: [List-of Post] -> [List-of Post]
;takes a list of posts and produces only posts that got 0 views (for Story), 0 likes (for FBpost and
; Tweet), or 0 retweets (for Tweet). The assignment also said to check for 0 "shares" but I'm not sure
;what that is since we were never instructed to make record a number of shares for Exercise 1.
(check-expect (sort-for-failed-posts '())
              '())
(check-expect (sort-for-failed-posts (list tweet-1 tweet-2 tweet-3
                                           fbpost-1 fbpost-2 fbpost-3
                                           story-1 story-2 story-3))
              '())
(check-expect (sort-for-failed-posts (list tweet-1 tweet-2 tweet-3
                                           fbpost-1 fbpost-2 fbpost-3
                                           story-1 story-2 story-3
                                           failed-tweet))
              (list failed-tweet ))
(check-expect (sort-for-failed-posts (list tweet-1 tweet-2 tweet-3
                                           fbpost-1 fbpost-2 fbpost-3
                                           story-1 story-2 story-3
                                           failed-tweet failed-fbpost failed-story))
              (list failed-tweet failed-fbpost failed-story))
(check-expect (sort-for-failed-posts (list failed-tweet failed-fbpost))
              (list failed-tweet failed-fbpost))
(define (sort-for-failed-posts lop)
  (filter failed-post? lop))

;failed-post?: Post -> Boolean
;did the given post get 0 likes or 0 retweets or 0 views?
(check-expect (failed-post? post-twitter) #f)
(check-expect (failed-post? failed-tweet) #t) ;note that failed-tweet, failed-story, and failed-post
;are in fact Posts, I just didn't feel like defining
;new posts specifically to work with these examples
(check-expect (failed-post? failed-fbpost) #t)
(check-expect (failed-post? failed-story) #t)
(define (failed-post? p)
  (cond [(tweet? p) (failed-tweet? p)]
        [(fbpost? p) (failed-fbpost-or-story? p fbpost-likes)] ;I could have just passed p which would
        ;be closer to the template but I
        ;figured why not practice abstraction?
        [(story? p) (failed-fbpost-or-story? p story-views)]))


;failed-tweet?: Tweet -> Boolean
;did the given tweet get 0 likes or retweets (inclusive or)?
(check-expect (failed-tweet? failed-tweet-0-0) #t)
(check-expect (failed-tweet? failed-tweet-0-1) #t)
(check-expect (failed-tweet? failed-tweet-1-0) #t)
(check-expect (failed-tweet? tweet-1) #f);,,obviously. 
(define (failed-tweet? t)
  (or (zero? (tweet-likes t))
      (zero? (tweet-retweets t))))

;failed-fbpost-or-story?: FBpost/Story [Struct -> Natural] -> Boolean
;                                          (a selector
;                                           selecting a
;                                             Natural)
;Does the given FBpost or Story have 0 likes?
(check-expect (failed-fbpost-or-story? failed-story story-views) #t)
(check-expect (failed-fbpost-or-story? story-1 story-views) #f)
(check-expect (failed-fbpost-or-story? failed-fbpost fbpost-likes) #t)
(check-expect (failed-fbpost-or-story? fbpost-1 fbpost-likes) #f)
(define (failed-fbpost-or-story? fbp/s selector)
  (zero? (selector fbp/s)))


;Exercise 5

;calculate-entergagement: [List-of Post] -> Natural
;calculates the sum of all likes, retweets, and views in a [List-of Post]
;if you're wondering why it's called entergagement, it's entertaining and engaging
;this is reference to the marketing of garfieldEATS, an app where you can order garfield-shaped
;(and only garfield-shaped) pizzas for delivery, it's wild.
;garfieldeats.com
(check-expect (calculate-entergagement (list tweet-1 fbpost-2 story-3))
              603740102)
(check-expect (calculate-entergagement (list tweet-1 tweet-2 tweet-3 failed-tweet))
              606365245)
(check-expect (calculate-entergagement (list failed-fbpost tweet-2 fbpost-left-authoritarian))
              2625243)
(check-expect (calculate-entergagement (list failed-tweet-0-0))
              0)
(check-expect (calculate-entergagement '())
              0)
(define (calculate-entergagement lop)
  (foldr + 0 (map post->entergagement lop)))

;post->entergagement-list: Post -> Natural
;selects the fields containing (the sum of) likes and retweets, likes, or views from a Tweet, FBpost,
;or Story, respectively.
(check-expect (post->entergagement post-twitter) 603740000)
(check-expect (post->entergagement failed-fbpost) 0)
(check-expect (post->entergagement post-story) 10000000)
(define (post->entergagement p)
  (cond [(tweet? p) (+ (tweet-likes p) (tweet-retweets p))] ;this is a basic calculation so I decided
        ;                                                        not to write a helper function for it
        [(fbpost? p) (fbpost-likes p)]
        [(story? p) (story-views p)]))


;Exercise 6

;crosspost: [List-of Post] -> [List-of Post]
;For each Post in the give [List-of Post], this function produces 2 new Posts of unique type with
;nearly identical content and 0 likes, retweets and likes, or views.
;For example, a Tweet in the list will produce a FBpost and Story with identical text and 0 likes or
;views. Note that Posts that produce Tweets must and will produce valid tweets of length <=280 chars.
(check-expect (crosspost (list fbpost-1 fbpost-2 tweet-3 story-1 story-2))
              (list
 (make-tweet "Oh gee, I love facebook" 0 0)
 (make-story "Oh gee, I love facebook" 0)
 (make-tweet "ooh boy, facebook am i right? aint it swell??" 0 0)
 (make-story "ooh boy, facebook am i right? aint it swell??" 0)
 (make-fbpost "NORMALIZE RABIES!!!!!!! GRRR!!!!!" 0)
 (make-story "NORMALIZE RABIES!!!!!!! GRRR!!!!!" 0)
 (make-tweet "You'll never believe this new medical secret doctors don't want you to\nknow!" 0 0)
 (make-fbpost "You'll never believe this new medical secret doctors don't want you to\nknow!" 0)
 (make-tweet "top 100 staircases in horror movies" 0 0)
 (make-fbpost "top 100 staircases in horror movies" 0)))
(check-expect (crosspost (list tweet-2 fbpost-2 story-2))
              (list
 (make-fbpost "Man ... ninjas are kind of cool ... I just don't know any personally" 0)
 (make-story "Man ... ninjas are kind of cool ... I just don't know any personally" 0)
 (make-tweet "ooh boy, facebook am i right? aint it swell??" 0 0)
 (make-story "ooh boy, facebook am i right? aint it swell??" 0)
 (make-tweet "top 100 staircases in horror movies" 0 0)
 (make-fbpost "top 100 staircases in horror movies" 0)))
(check-expect (crosspost '()) '())
(define (crosspost lop)
  (cond [(empty? lop) '()]
        [(cons? lop) (append (generate-crossposts (first lop))
                             (crosspost (rest lop)))]))

;generate-crossposts: Post -> [List-of Post]
;Takes a single Post and generates two posts of a different type with 0 likes, retweets and likes, or
;views. Always generates a list of exactly 2 Posts
(check-expect (generate-crossposts tweet-1)
              (list
 (make-fbpost
  "I will be leaving the great Walter Reed Medical Center today at 6:30 P.M.\n Feeling really good! Don’t be afraid of Covid. Don’t let it dominate your life. We have developed,\nunder the Trump Administration, some really great drugs & knowledge. I feel better than I did 20 years\n ago!"
  0)
 (make-story
  "I will be leaving the great Walter Reed Medical Center today at 6:30 P.M.\n Feeling really good! Don’t be afraid of Covid. Don’t let it dominate your life. We have developed,\nunder the Trump Administration, some really great drugs & knowledge. I feel better than I did 20 years\n ago!"
  0)))
(check-expect (generate-crossposts fbpost-1)
              (list
 (make-tweet "Oh gee, I love facebook" 0 0)
 (make-story "Oh gee, I love facebook" 0)))
(check-expect (generate-crossposts story-1)
              (list
 (make-tweet
  "You'll never believe this new medical secret doctors don't want you to\nknow!"
  0
  0)
 (make-fbpost
  "You'll never believe this new medical secret doctors don't want you to\nknow!"
  0)))
(define (generate-crossposts p)
  (cond [(tweet? p) (tweet->fbpost&story p)]
          [(fbpost? p) (fbpost->tweet&story p)]
          [(story? p) (story->tweet&fbpost p)]))

;tweet->fbpost&story: Tweet -> [List-of Post]
;takes a Tweet and generates a list containing a FBpost and Story with identical text and 0 likes and
;views respectively.
(check-expect (tweet->fbpost&story tweet-1)
              (list
 (make-fbpost
  "I will be leaving the great Walter Reed Medical Center today at 6:30 P.M.\n Feeling really good! Don’t be afraid of Covid. Don’t let it dominate your life. We have developed,\nunder the Trump Administration, some really great drugs & knowledge. I feel better than I did 20 years\n ago!"
  0)
 (make-story
  "I will be leaving the great Walter Reed Medical Center today at 6:30 P.M.\n Feeling really good! Don’t be afraid of Covid. Don’t let it dominate your life. We have developed,\nunder the Trump Administration, some really great drugs & knowledge. I feel better than I did 20 years\n ago!"
  0)))
(check-expect (tweet->fbpost&story tweet-2)
              (list
 (make-fbpost
  "Man ... ninjas are kind of cool ... I just don't know any personally"
  0)
 (make-story
  "Man ... ninjas are kind of cool ... I just don't know any personally"
  0)))
(check-expect (tweet->fbpost&story tweet-3)
              (list
 (make-fbpost "NORMALIZE RABIES!!!!!!! GRRR!!!!!" 0)
 (make-story "NORMALIZE RABIES!!!!!!! GRRR!!!!!" 0)))
(define (tweet->fbpost&story t)
  (list (make-fbpost (tweet-text t) 0)
        (make-story (tweet-text t) 0)))

;fbpost->tweet&story: FBpost -> [List-of Post]
;takes a FBpost and generates a list containing a Tweet and Story with identical text and 0 likes
;retweets and views respectively.
;Note that the text of a Tweet will be limited to at most 280 chars
(check-expect (fbpost->tweet&story fbpost-1) (list
 (make-tweet "Oh gee, I love facebook" 0 0)
 (make-story "Oh gee, I love facebook" 0)))
(check-expect (fbpost->tweet&story fbpost-2) (list
 (make-tweet "ooh boy, facebook am i right? aint it swell??" 0 0)
 (make-story "ooh boy, facebook am i right? aint it swell??" 0)))
(check-expect (fbpost->tweet&story fbpost-3) (list
 (make-tweet "golly gee jeepers! Facebook! Wowee!!!" 0 0)
 (make-story "golly gee jeepers! Facebook! Wowee!!!" 0)))
(check-expect (fbpost->tweet&story (make-fbpost ts-281-chars 0))
              (list
 (make-tweet
  "blablablablablablabla\nblablablablablablabla\nblablablablablablabla\nblablablablablablabla\nblablablablablablabla\nblablablablablablabla\nblablablablablablabla\n\nblablablablablablabla\nblablablablablablabla\nblablablablablablabla\nBlablablablablablablablablablablabTHIS IS THE  281st char: "
  0
  0)
 (make-story
  "blablablablablablabla\nblablablablablablabla\nblablablablablablabla\nblablablablablablabla\nblablablablablablabla\nblablablablablablabla\nblablablablablablabla\n\nblablablablablablabla\nblablablablablablabla\nblablablablablablabla\nBlablablablablablablablablablablabTHIS IS THE  281st char: !"
  0)))
(define (fbpost->tweet&story fbp)
  (list (make-tweet (shorten-text-for-tweet (fbpost-text fbp)) 0 0) ;the assignment didn't specify how
                    ;many retweets the tweet
                    ;should have, so I'm assuming zero
        (make-story (fbpost-text fbp) 0)))

;story->tweet&fbpost: Story -> [List-of Post]
;takes a Story and generates a list containing a Tweet and FBpost with identical text and 0 likes and
;retweets and views respectively.
;Note that the text of a Tweet will be limited to at most 280 chars
(check-expect (story->tweet&fbpost story-1) (list
 (make-tweet
  "You'll never believe this new medical secret doctors don't want you to\nknow!"
  0
  0)
 (make-fbpost
  "You'll never believe this new medical secret doctors don't want you to\nknow!"
  0)))
(check-expect (story->tweet&fbpost story-2) (list
 (make-tweet "top 100 staircases in horror movies" 0 0)
 (make-fbpost "top 100 staircases in horror movies" 0)))
(check-expect (story->tweet&fbpost story-3) (list
 (make-tweet "this is a story online" 0 0)
 (make-fbpost "this is a story online" 0)))
(check-expect (story->tweet&fbpost (make-story ts-281-chars 0))
              (list
 (make-tweet
  "blablablablablablabla\nblablablablablablabla\nblablablablablablabla\nblablablablablablabla\nblablablablablablabla\nblablablablablablabla\nblablablablablablabla\n\nblablablablablablabla\nblablablablablablabla\nblablablablablablabla\nBlablablablablablablablablablablabTHIS IS THE  281st char: "
  0
  0)
 (make-fbpost
  "blablablablablablabla\nblablablablablablabla\nblablablablablablabla\nblablablablablablabla\nblablablablablablabla\nblablablablablablabla\nblablablablablablabla\n\nblablablablablablabla\nblablablablablablabla\nblablablablablablabla\nBlablablablablablablablablablablabTHIS IS THE  281st char: !"
  0)))
(define (story->tweet&fbpost s)
  (list (make-tweet (shorten-text-for-tweet (story-text s)) 0 0)
        (make-fbpost (story-text s) 0)))

;shorten-text-for-tweet: String -> String
;Shortens a string to be at most 280 chars
;THESE ARE CORRECT IDK WHY THEYRE NOT WORKING
#;(check-expect (shorten-text-for-tweet ts-280-chars) "blablablablablablabla\nblablablablablablabla\nb
lablablablablablabla\nblablablablablablabla\nblablablablablablabla\nblablablablablablabla\nblablablabl
ablablabla\n\nblablablablablablabla\nblablablablablablabla\nblablablablablablabla\nBlablablablablablab
lablablablablabTHIS IS THE 280th char: !")
#;(check-expect (shorten-text-for-tweet ts-281-chars) "blablablablablablabla\nblablablablablablabla\nbla
blablablablablabla\nblablablablablablabla\nblablablablablablabla\nblablablablablablabla\nblablablablab
lablabla\n\nblablablablablablabla\nblablablablablablabla\nblablablablablablabla\nBlablablablablablabla
blablablablabTHIS IS THE  281st char: ")
(check-expect (shorten-text-for-tweet "bla") "bla")
(define (shorten-text-for-tweet text)
  (if (>= (string-length text) TS-MAX-LENGTH)
      (substring text 0 280)
      text))


;Exercise 7

;append-apply-to-all: (X Y) [X -> [List-of Y]] [List-of X] -> [List-of Y]
;applies a function _f_ to all elements in a list _l_ and appends the lists that result from each
;application of _f_ to each element in list _l_
(check-expect (append-apply-to-all string->list (list "hi" "ta" "how" "are" "you" "doing today?"))
              (list #\h #\i #\t #\a #\h #\o #\w #\a #\r #\e #\y #\o #\u #\d #\o #\i #\n #\g #\space
                    #\t #\o #\d #\a #\y #\?))
(check-expect (append-apply-to-all next-three (list 1 2 3 4 5))
                (list 2 3 4 3 4 5 4 5 6 5 6 7 6 7 8))
(check-expect (append-apply-to-all next-three '()) '())
;TESTING FUNCTIONS
;----------------------------------------
;next-three: Integer -> [List-of Integer]
;given a number, produces a list of the next three consecutive integers
(check-expect (next-three 1) (list 2 3 4))
(check-expect (next-three -103) (list -102 -101 -100))
(check-expect (next-three -2) (list -1 0 1))
(define (next-three n) 
          (list (+ n 1) (+ n 2) (+ n 3)))
;----------------------------------------
(define (append-apply-to-all f l)
  (foldr append '() (map f l)))


;Exercise 8

;crosspost/v2: [List-of Post] -> [List-of Post]
;implements the abstraction append-apply-to-all to do the same as crosspost (see crosspost)
(check-expect (crosspost/v2 (list fbpost-1 fbpost-2 tweet-3 story-1 story-2))
              (list
 (make-tweet "Oh gee, I love facebook" 0 0)
 (make-story "Oh gee, I love facebook" 0)
 (make-tweet "ooh boy, facebook am i right? aint it swell??" 0 0)
 (make-story "ooh boy, facebook am i right? aint it swell??" 0)
 (make-fbpost "NORMALIZE RABIES!!!!!!! GRRR!!!!!" 0)
 (make-story "NORMALIZE RABIES!!!!!!! GRRR!!!!!" 0)
 (make-tweet "You'll never believe this new medical secret doctors don't want you to\nknow!" 0 0)
 (make-fbpost "You'll never believe this new medical secret doctors don't want you to\nknow!" 0)
 (make-tweet "top 100 staircases in horror movies" 0 0)
 (make-fbpost "top 100 staircases in horror movies" 0)))
(check-expect (crosspost/v2 (list tweet-2 fbpost-2 story-2))
              (list
 (make-fbpost "Man ... ninjas are kind of cool ... I just don't know any personally" 0)
 (make-story "Man ... ninjas are kind of cool ... I just don't know any personally" 0)
 (make-tweet "ooh boy, facebook am i right? aint it swell??" 0 0)
 (make-story "ooh boy, facebook am i right? aint it swell??" 0)
 (make-tweet "top 100 staircases in horror movies" 0 0)
 (make-fbpost "top 100 staircases in horror movies" 0)))
(check-expect (crosspost/v2 '()) '())
(define (crosspost/v2 lop)
  (append-apply-to-all generate-crossposts lop))


;Exercise 9

;items-since-tweet: [List-of Post] TweetString-> [List-of Post]
;returns all of the Posts after the Post containing the given TweetString
(check-expect (items-since-tweet (list tweet-1 tweet-2 tweet-2 fbpost-2 story-1)
                                 (tweet-text tweet-2))
              (list tweet-2 tweet-2 fbpost-2 story-1))
(check-expect (items-since-tweet (list tweet-1 tweet-2 tweet-2 fbpost-2 story-1)
                                 (fbpost-text fbpost-2))
              '())
(check-expect (items-since-tweet '() (tweet-text tweet-1))
              '())
(check-expect (items-since-tweet (list tweet-1 story-1 fbpost-1)
                                 "this string is in none of the above posts")
              '())
(define (items-since-tweet lop text)
  (cond [(empty? lop) '()]
        [(cons? lop) (if (and (tweet? (first lop))
                              (string=? (tweet-text (first lop)) text))
                         (cons (first lop) (rest lop))
                         (items-since-tweet (rest lop) text))]))


;items-since-10-likes: [List-of Post] -> [List-of Post]
;given a list of Posts, produces a list containing all the Posts after an FBpost that got at least 10
;likes, including that FBpost as well
(define (items-since-10-likes lop)
  (cond [(empty? lop) '()]
        [(cons? lop) (if (and (fbpost? (first lop))
                              (>= (fbpost-likes (first lop)) 10))
                         (cons (first lop) (rest lop))
                         (items-since-10-likes (rest lop)))]))

;Exercise 11

;suffix-from-2500: [List-of Number] -> [List-of Number]
;given a list of Numbers, produces a list containing all of the numbers appearing after the number
;2500, including that number 2500 itself
(define (suffix-from-2500 lon)
  (cond [(empty? lon) '()]
        [(cons? lon) (if (= (first lon) 2500)
                         (cons (first lon) (rest lon))
                         (suffix-from-2500 (rest lon)))]))

;Exercise 12

;chop-list: (X Y) [List-of X] [X -> Boolean] [Y Y ... Y -> Boolean] [X -> Y] Y -> [List-of X]
;PURPOSE
(define (chop-list lox x-type? compare-y transform-x compared-y-val)
  (cond [(empty? lox) '()]
        [(cons? lox) (if (and (x-type? (first lox))
                              (compare-y (transform-x (first lox)) compared-y-val))
                         (cons (first lox) (rest lox))
                         (chop-list (rest lox) x-type? compare-y transform-x compared-y-val))]))

;Exercise 13

;items-since-tweet/v2: [List-of Post] -> [List-of Post]
;implements chop-list to rewrite items-since-tweet
(define (items-since-tweet/v2 lop text)
  (chop-list lop tweet? string=? tweet-text text))

;items-since-10-likes/v2: [List-of Post] -> [List-of Post]
;implements chop-list to rewrite items-since-10-likes
(define (items-since-10-likes/v2 lop)
  (chop-list lop fbpost? = fbpost-likes 10))

;suffix-from-2500/v2
;implements chop-list to rewrite suffix-from-2500
(define (suffix-from-2500/v2 lon)
  (chop-list lon number? = identity 2500))
