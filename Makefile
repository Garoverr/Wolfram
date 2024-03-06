##
## EPITECH PROJECT, 2024
## MakeStack
## File description:
## makefile for WOLFRAM project
##


BINARY_PATH 	:=	$(shell stack path --local-install-root)

NAME 		= 	wolfram

all	:
		stack build
		cp $(BINARY_PATH)/bin/$(NAME)-exe ./$(NAME)

clean	:
		stack clean

fclean	:	clean
		rm -f $(NAME)

re	:	fclean all

.PHONY	:	all clean fclean re
