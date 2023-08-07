render_book(input = "index.Rmd", output_format = "bookdown::pdf_book",
            output_dir = "Output/pdf_book",
            config_file = "_bookdown.yml")

render_book(input = "index.Rmd", output_format = "bookdown::bs4_book",
            output_dir = "docs",
            config_file = "_bookdown.yml")
