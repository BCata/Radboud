module assignment6

import iTasks
import StdArray

derive class iTask Idea
derive class iTask NamedIdea
derive class iTask PreviewIdea

:: Name :== String

:: Idea	= 
	{ idea_name :: String
	, idea_description :: String
	}
	
:: NamedIdea = 
	{ number :: Int
	, idea :: Idea
	, user :: String
	}
	
:: PreviewIdea = 
	{ num :: Int
	, idea_title :: String
	, owner :: String
	}
	
ideas :: Shared [NamedIdea]
ideas = sharedStore "Ideas" []


editIdeas :: Name [NamedIdea] -> Task [NamedIdea]
editIdeas name list
	| name == "admin" = adminTask
	| otherwise		  = updateInformation ("Enter your idea " +++ name) 
						[] 
						{idea_name = "", idea_description = ""}
						>>*	[OnAction ActionOk (ifValue isValid (\v->return(v) 
									>>=	\idea.set (list++[{number=length list + 1,idea=idea,user=name}]) ideas
				 					>>= \namedIdeas.editIdeas name namedIdeas))
							,OnAction (Action "Delete" []) (always (deleteAllIdeas name list))
							,OnAction (Action "Clear" []) (always (editIdeas name list))
							]
where isValid s = size s.idea_description >= 3 && s.idea_name <> s.idea_description

deleteAllIdeas :: Name [NamedIdea] -> Task [NamedIdea]
deleteAllIdeas name list
			= 	viewInformation "Are you sure ?" [] name
			>>*	[OnAction (Action "Yes" []) (always (set [] ideas >>= \_.editIdeas name []))
				,OnAction (Action "No" []) (always (editIdeas name list))
				]

adminTask :: Task [NamedIdea]
adminTask = updateSharedInformation "Update ideas" [] ideas

displayIdea :: Task NamedIdea
displayIdea
		= enterChoiceWithShared "Current idea list" [ChooseWith (ChooseFromGrid conv)] ideas
		>&^ \id.viewSharedInformation "Selected idea" [] id
where conv s = {num = s.number, idea_title = s.idea.idea_name, owner = s.user}

loginUser :: Task String
loginUser = enterInformation "Enter your name" []

go :: Task [NamedIdea]
go = loginUser >>= \name.editIdeas name [] -|| displayIdea

Start :: *World -> *World
Start world = startEngine go world