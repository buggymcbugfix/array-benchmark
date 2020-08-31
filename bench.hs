
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Criterion.Main

-- import Helpers
-- import MkSmallArray
-- import MkSmallArrayOld
import Array
-- import GHC.Exts
-- -- UNIT TEST
-- main = do
--   print $ map $$(new 5) [0..4]
--   print $ map $$(old 5) [0..4]
--   print $ map (dynam 5) [0..4]


setupEnv :: IO [Array Int]
setupEnv = do
	let mk n = fromList n (replicate n 0)
	pure [mk 0, mk 1, mk 7, mk 47, mk 223, mk 3967, mk 16127, mk 1046527]

main :: IO ()
main = defaultMain
	[ env setupEnv $ \ ~[a0, a1, a7, a47, a223, a3967, a16127, a1046527] -> bgroup "insert"
		[ bench "old/a0/0"                       $ whnf (insert         a0             0) undefined
		, bench "new/a0/0"                       $ whnf (insert_new     a0             0) undefined

		, bench "old/a1/0"                       $ whnf (insert         a1             0) undefined
		, bench "new/a1/0"                       $ whnf (insert_new     a1             0) undefined

		, bench "old/a7/0"                       $ whnf (insert         a7             0) undefined
		, bench "new/a7/0"                       $ whnf (insert_new     a7             0) undefined
		, bench "old/a7/3"                       $ whnf (insert         a7             3) undefined
		, bench "new/a7/3"                       $ whnf (insert_new     a7             3) undefined
		, bench "old/a7/6"                       $ whnf (insert         a7             6) undefined
		, bench "new/a7/6"                       $ whnf (insert_new     a7             6) undefined

		, bench "old/a47/0"                      $ whnf (insert         a47            0) undefined
		, bench "new/a47/0"                      $ whnf (insert_new     a47            0) undefined
		, bench "old/a47/23"                     $ whnf (insert         a47           23) undefined
		, bench "new/a47/23"                     $ whnf (insert_new     a47           23) undefined
		, bench "old/a47/46"                     $ whnf (insert         a47           46) undefined
		, bench "new/a47/46"                     $ whnf (insert_new     a47           46) undefined

		, bench "old/a223/0"                     $ whnf (insert         a223           0) undefined
		, bench "new/a223/0"                     $ whnf (insert_new     a223           0) undefined
		, bench "old/a223/111"                   $ whnf (insert         a223         111) undefined
		, bench "new/a223/111"                   $ whnf (insert_new     a223         111) undefined
		, bench "old/a223/222"                   $ whnf (insert         a223         222) undefined
		, bench "new/a223/222"                   $ whnf (insert_new     a223         222) undefined

		, bench "old/a3967/0"                    $ whnf (insert         a3967          0) undefined
		, bench "new/a3967/0"                    $ whnf (insert_new     a3967          0) undefined
		, bench "old/a3967/1983"                 $ whnf (insert         a3967       1983) undefined
		, bench "new/a3967/1983"                 $ whnf (insert_new     a3967       1983) undefined
		, bench "old/a3967/3966"                 $ whnf (insert         a3967       3966) undefined
		, bench "new/a3967/3966"                 $ whnf (insert_new     a3967       3966) undefined

		, bench "old/a16127/0"                   $ whnf (insert         a16127         0) undefined
		, bench "new/a16127/0"                   $ whnf (insert_new     a16127         0) undefined
		, bench "old/a16127/8063"                $ whnf (insert         a16127      8063) undefined
		, bench "new/a16127/8063"                $ whnf (insert_new     a16127      8063) undefined
		, bench "old/a16127/16126"               $ whnf (insert         a16127     16126) undefined
		, bench "new/a16127/16126"               $ whnf (insert_new     a16127     16126) undefined

		, bench "old/a1046527/0"                 $ whnf (insert         a1046527       0) undefined
		, bench "new/a1046527/0"                 $ whnf (insert_new     a1046527       0) undefined
		, bench "old/a1046527/523263"            $ whnf (insert         a1046527  523263) undefined
		, bench "new/a1046527/523263"            $ whnf (insert_new     a1046527  523263) undefined
		, bench "old/a10465271046527/"           $ whnf (insert         a1046527 1046527) undefined
		, bench "new/a10465271046527/"           $ whnf (insert_new     a1046527 1046527) undefined
		]

	, env setupEnv $ \ ~[a0, a1, a7, a47, a223, a3967, a16127, a1046527] -> bgroup "update"
		[ bench "old/a0/0"                       $ whnf (update         a0             0) undefined
		, bench "new/a0/0"                       $ whnf (update_new     a0             0) undefined

		, bench "old/a1/0"                       $ whnf (update         a1             0) undefined
		, bench "new/a1/0"                       $ whnf (update_new     a1             0) undefined

		, bench "old/a7/0"                       $ whnf (update         a7             0) undefined
		, bench "new/a7/0"                       $ whnf (update_new     a7             0) undefined
		, bench "old/a7/3"                       $ whnf (update         a7             3) undefined
		, bench "new/a7/3"                       $ whnf (update_new     a7             3) undefined
		, bench "old/a7/6"                       $ whnf (update         a7             6) undefined
		, bench "new/a7/6"                       $ whnf (update_new     a7             6) undefined

		, bench "old/a47/0"                      $ whnf (update         a47            0) undefined
		, bench "new/a47/0"                      $ whnf (update_new     a47            0) undefined
		, bench "old/a47/23"                     $ whnf (update         a47           23) undefined
		, bench "new/a47/23"                     $ whnf (update_new     a47           23) undefined
		, bench "old/a47/46"                     $ whnf (update         a47           46) undefined
		, bench "new/a47/46"                     $ whnf (update_new     a47           46) undefined

		, bench "old/a223/0"                     $ whnf (update         a223           0) undefined
		, bench "new/a223/0"                     $ whnf (update_new     a223           0) undefined
		, bench "old/a223/111"                   $ whnf (update         a223         111) undefined
		, bench "new/a223/111"                   $ whnf (update_new     a223         111) undefined
		, bench "old/a223/222"                   $ whnf (update         a223         222) undefined
		, bench "new/a223/222"                   $ whnf (update_new     a223         222) undefined

		, bench "old/a3967/0"                    $ whnf (update         a3967          0) undefined
		, bench "new/a3967/0"                    $ whnf (update_new     a3967          0) undefined
		, bench "old/a3967/1983"                 $ whnf (update         a3967       1983) undefined
		, bench "new/a3967/1983"                 $ whnf (update_new     a3967       1983) undefined
		, bench "old/a3967/3966"                 $ whnf (update         a3967       3966) undefined
		, bench "new/a3967/3966"                 $ whnf (update_new     a3967       3966) undefined

		, bench "old/a16127/0"                   $ whnf (update         a16127         0) undefined
		, bench "new/a16127/0"                   $ whnf (update_new     a16127         0) undefined
		, bench "old/a16127/8063"                $ whnf (update         a16127      8063) undefined
		, bench "new/a16127/8063"                $ whnf (update_new     a16127      8063) undefined
		, bench "old/a16127/16126"               $ whnf (update         a16127     16126) undefined
		, bench "new/a16127/16126"               $ whnf (update_new     a16127     16126) undefined

		, bench "old/a1046527/0"                 $ whnf (update         a1046527       0) undefined
		, bench "new/a1046527/0"                 $ whnf (update_new     a1046527       0) undefined
		, bench "old/a1046527/523263"            $ whnf (update         a1046527  523263) undefined
		, bench "new/a1046527/523263"            $ whnf (update_new     a1046527  523263) undefined
		, bench "old/a10465271046527/"           $ whnf (update         a1046527 1046527) undefined
		, bench "new/a10465271046527/"           $ whnf (update_new     a1046527 1046527) undefined
		]

	, env setupEnv $ \ ~[a0, a1, a7, a47, a223, a3967, a16127, a1046527] -> bgroup "delete"
		[ bench "old/a0/0"                       $ whnf (delete         a0      )       0
		, bench "new/a0/0"                       $ whnf (delete_new     a0      )       0

		, bench "old/a1/0"                       $ whnf (delete         a1      )       0
		, bench "new/a1/0"                       $ whnf (delete_new     a1      )       0

		, bench "old/a7/0"                       $ whnf (delete         a7      )       0
		, bench "new/a7/0"                       $ whnf (delete_new     a7      )       0
		, bench "old/a7/3"                       $ whnf (delete         a7      )       3
		, bench "new/a7/3"                       $ whnf (delete_new     a7      )       3
		, bench "old/a7/6"                       $ whnf (delete         a7      )       6
		, bench "new/a7/6"                       $ whnf (delete_new     a7      )       6

		, bench "old/a47/0"                      $ whnf (delete         a47     )       0
		, bench "new/a47/0"                      $ whnf (delete_new     a47     )       0
		, bench "old/a47/23"                     $ whnf (delete         a47     )      23
		, bench "new/a47/23"                     $ whnf (delete_new     a47     )      23
		, bench "old/a47/46"                     $ whnf (delete         a47     )      46
		, bench "new/a47/46"                     $ whnf (delete_new     a47     )      46

		, bench "old/a223/0"                     $ whnf (delete         a223    )       0
		, bench "new/a223/0"                     $ whnf (delete_new     a223    )       0
		, bench "old/a223/111"                   $ whnf (delete         a223    )     111
		, bench "new/a223/111"                   $ whnf (delete_new     a223    )     111
		, bench "old/a223/222"                   $ whnf (delete         a223    )     222
		, bench "new/a223/222"                   $ whnf (delete_new     a223    )     222

		, bench "old/a3967/0"                    $ whnf (delete         a3967   )       0
		, bench "new/a3967/0"                    $ whnf (delete_new     a3967   )       0
		, bench "old/a3967/1983"                 $ whnf (delete         a3967   )    1983
		, bench "new/a3967/1983"                 $ whnf (delete_new     a3967   )    1983
		, bench "old/a3967/3966"                 $ whnf (delete         a3967   )    3966
		, bench "new/a3967/3966"                 $ whnf (delete_new     a3967   )    3966

		, bench "old/a16127/0"                   $ whnf (delete         a16127  )       0
		, bench "new/a16127/0"                   $ whnf (delete_new     a16127  )       0
		, bench "old/a16127/8063"                $ whnf (delete         a16127  )    8063
		, bench "new/a16127/8063"                $ whnf (delete_new     a16127  )    8063
		, bench "old/a16127/16126"               $ whnf (delete         a16127  )   16126
		, bench "new/a16127/16126"               $ whnf (delete_new     a16127  )   16126

		, bench "old/a1046527/0"                 $ whnf (delete         a1046527)       0
		, bench "new/a1046527/0"                 $ whnf (delete_new     a1046527)       0
		, bench "old/a1046527/523263"            $ whnf (delete         a1046527)  523263
		, bench "new/a1046527/523263"            $ whnf (delete_new     a1046527)  523263
		, bench "old/a10465271046527/"           $ whnf (delete         a1046527) 1046527
		, bench "new/a10465271046527/"           $ whnf (delete_new     a1046527) 1046527
		]
	]


	-- 		$$(let ns = 1 : [50..1000] in -- change iterations here
	-- 				listTE $ concatMap (\(w,x,y,z) -> [w,x,y,z]) $ zip4
	-- 					(map (\n -> [|| bench ("arrayOf-static/" <> show n) $ nf @Int $$(new n) (n - 1) ||]) ns)
	-- 					[ [|| bench ("arrayOf/1") $ nf @Int (\i@(I# i#) -> case indexSmallArray# (smallArrayOf# (# i, 1 #)) i# of (# k #) -> k) 0||]

	-- 					(map (\n -> [|| bench ("template-haskell/" <> show n) $ nf @Int $$(old n) (n - 1) ||]) ns)
	-- 					(map (\n -> [|| bench ("dynamic/" <> show n) $ nf @Int (dynam n) (n - 1) ||]) ns)
	-- 			)
	-- ]
