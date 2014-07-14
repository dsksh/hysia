#define p true

never { /* Gp */
accept_init:
  if
  :: ((p)) -> goto accept_init
  fi;
}
