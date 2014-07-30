# spell checkd comments are great!
# It also highlights code objects like TermX 

class TermX(object):
    """
    A mathematical Term object
    """
    def __init__(self, *args):
        for i in args:
            setattr(self,i,args[i])

    def __new__(self):
        """
        Whoops, I'm going to need to implement this.
        """
        
        
        
        

