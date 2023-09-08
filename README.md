# rizek virtual machine

what

At this point you need to program it in c. not that the virtual machine would accept c, but its only way to generate binary format that rizek accepts.

## Register standard

You can technicaly use any register to do anything you want, but standartization is good

| register | usage | 
| 0 | stack pointer |
| 1 | stack low limit pointer |
| 2 | variable address | 
| 3 | variable output | 
| 4 | low level helping register |
| 5 | expression value register |
| 15 | zero |

and the rest is for arguments and stuff like that

## TODO 

syscalls
