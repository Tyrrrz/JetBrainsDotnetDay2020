module Qry.Books

type Book = {
    Title : string
    Author : string
    Category : string
    PublishYear : int
    Rating : float
}

let getAll() = [
    {
        Title = "The Fellowship of the Ring"
        Author = "J.R.R. Tolkien"
        Category = "Fantasy"
        PublishYear = 1954
        Rating = 4.36
    }
    {
        Title = "Harry Potter and the Philosopher's Stone"
        Author = "J.K. Rowling"
        Category = "Fantasy"
        PublishYear = 1997
        Rating = 4.47
    }
    {
        Title = "A Darker Shade of Magic"
        Author = "V.E. Schwab"
        Category = "Fiction"
        PublishYear = 2015
        Rating = 4.09
    }
    {
        Title = "My Not So Perfect Life"
        Author = "Sophie Kinsella"
        Category = "Contemporary"
        PublishYear = 2017
        Rating = 3.8
    }
    {
        Title = "Recursion"
        Author = "Blake Crouch"
        Category = "Science Fiction"
        PublishYear = 2019
        Rating = 4.18
    }
    {
        Title = "Read Bottom Up"
        Author = "Neel Shah, Skye Chatham"
        Category = "Romance"
        PublishYear = 2015
        Rating = 3.47
    }
    {
        Title = "My Grammar and I... Or Should That Be Me?: How to Speak and Write It Right"
        Author = "Caroline Taggart"
        Category = "Nonfiction"
        PublishYear = 2009
        Rating = 3.84
    }
]