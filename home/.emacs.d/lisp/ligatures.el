;; -*- lexical-binding: t; -*-

(ligature-set-ligatures '(prog-mode) (mapcar #'(lambda (x) (if (symbolp x) (symbol-name x) x))
				                             `(-> <- => =>> >=> =>=
				                                  =<< =<= <=< <=> >> >>>
				                                  << <<< <> <|> == ===
				                                  .= := "#=" != !== =!=
				                                  =:= :: ::: :<: :>: ||
				                                  |> ||> |||> <| <|| <|||
				                                  ** *** <* <*> *> <+
				                                  <+> +> <$ <$> $> &&
				                                  ?? %% "[|" "|]" // ///)))

(ligature-set-ligatures '(org-mode) (mapcar #'(lambda (x) (if (symbolp x) (symbol-name x) x))
				                            `(-> <- => =>> >=> =>=
				                                 =<< =<= <=< <=> >> >>>
				                                 << <<< <> <|> == ===
				                                 .= := "#=" != !== =!=
				                                 =:= :: ::: :<: :>: ||
				                                 |> ||> |||> <| <||
                                                 <||| <* <*> *> <+
				                                 <+> +> <$ <$> $> &&
				                                 ?? %% "[|" "|]" // ///)))

(global-ligature-mode 1)
