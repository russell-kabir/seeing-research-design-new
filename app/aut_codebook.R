# =============================================================================
# Alternate Uses Task codebook (Wahbeh et al., preprint, "Creative self-belief
# responses versus manual and automated Guilford alternate use task scoring",
# Supplemental Data: Alternate Use Task Scoring Codebook). Categories, elaboration
# anchors and originality exemplars are taken from that codebook; Japanese labels and
# the Japanese keyword hints (kw) were added for this app and only *suggest* a category.
# A few exemplars are lightly shortened; check the original codebook for exact wording.
# Published as: Wahbeh, H., Cannard, C., Yount, G., Delorme, A., & Radin, D. (2024).
# Journal of Creativity, 34(3), 100088.
# =============================================================================

cat_ <- function(en, ja, kw) list(en = en, ja = ja, kw = kw)
ex_ <- function(en, ja) c(en, ja)

AUT_CODEBOOK <- list(
  newspaper = list(
    en = "Newspaper", ja = "新聞",
    aliases = c("newspaper", "newspapers", "news paper", "新聞", "新聞紙", "しんぶん", "シンブン"),
    cats = list(
      cat_("Kindling / fire starting", "焚き付け・火おこし", c("火", "燃", "焚", "たき", "着火", "kindling", "fire", "burn")),
      cat_("Reading (knowledge, gaining information)", "読む（知識・情報を得る）", c("読", "情報", "ニュース", "記事", "知る", "勉強", "学", "read", "news", "learn")),
      cat_("Stationery (writing, craft, stacking)", "文具（書く・工作・積む）", c("書", "メモ", "積", "write", "note", "stack")),
      cat_("Specific crafts (origami, collage, papier-mâché, confetti)", "特定の工作（折り紙・コラージュ・張り子・紙吹雪）", c("折り紙", "折", "かぶと", "兜", "張り子", "紙粘土", "コラージュ", "紙吹雪", "origami", "papier", "mache", "collage", "confetti")),
      cat_("Organization (sorting, isolation)", "整理（分ける・仕切る）", c("整理", "分け", "仕切", "区別", "sort", "organi")),
      cat_("Packing / lining / insulation / covering", "梱包・敷く・保温・覆う", c("包", "梱包", "緩衝", "詰め", "敷", "保温", "断熱", "防寒", "覆", "カバー", "pack", "wrap", "line", "insulat", "cover")),
      cat_("Weapon (throwing, hitting, violence)", "武器（投げる・たたく・暴力）", c("武器", "たたく", "叩", "殴", "投げ", "攻撃", "ハエ", "虫", "weapon", "hit", "swat", "throw")),
      cat_("Stabilization (sit on, under a table leg)", "安定させる（座る・机の脚の下に）", c("座", "下に", "支え", "固定", "安定", "がたつき", "sit", "stabil", "under")),
      cat_("Getting rid of / giving (recycle, gift, shred)", "処分・あげる（リサイクル・寄付・シュレッダー）", c("リサイクル", "捨", "寄付", "あげ", "売", "古紙", "シュレッダー", "recycl", "gift", "donat", "shred", "sell")),
      cat_("Cleaning (wipe up messes, clean windows)", "掃除（汚れを拭く・窓をきれいに）", c("拭", "ふく", "掃除", "窓", "汚れ", "磨", "油", "clean", "wipe", "window")),
      cat_("Weight (lift, bench press)", "重り（持ち上げる・ベンチプレス）", c("重し", "おもし", "重り", "筋トレ", "持ち上げ", "weight", "lift")),
      cat_("Garden (mulch, planter lining, weed prevention)", "庭・畑（マルチ・プランターの敷物・雑草よけ）", c("庭", "畑", "雑草", "マルチ", "堆肥", "植", "プランター", "garden", "mulch", "weed", "compost"))),
    elab = list(ex_("“To read”, “kindling”, “packing”", "「読む」「焚き付け」「梱包」"),
                ex_("A 2–4 word phrase", "2〜4語ほどの句"),
                ex_("“Blocking wind below a door”, “keep the cat off the couch”, “fan yourself on a hot day”",
                    "「ドアの下のすき間風を防ぐ」「猫をソファに乗せないようにする」「暑い日にあおぐ」")),
    orig = list(
      `0` = list(ex_("Reading / information", "読む・情報"), ex_("Kindling / fire starting", "焚き付け"), ex_("Packing material", "梱包材"),
                 ex_("Wrapping", "包む"), ex_("Folding", "折る"), ex_("Throw away / recycle", "捨てる・リサイクル"), ex_("Pet clean up", "ペットの後始末"),
                 ex_("Window cleaning", "窓ふき"), ex_("Stacking, booster seat", "積む・座面を高くする"), ex_("Art, papier-mâché, collages", "アート・張り子・コラージュ"),
                 ex_("Games, play (with people or animals)", "ゲーム・遊び（人や動物と）"), ex_("Using as a table, chair or shelf", "机・椅子・棚として使う"),
                 ex_("Covering a window / shade", "窓をおおう・日よけ"), ex_("Weight", "重し"), ex_("Throwing, scattering", "投げる・まく"),
                 ex_("Protect the floor from paint, oil, etc.", "ペンキや油から床を守る"), ex_("Gardening, weed control, compost", "園芸・雑草よけ・堆肥"),
                 ex_("Discussion with others", "人と話し合う"), ex_("Fan", "うちわ"), ex_("Self-defense, throw at someone, projectile", "護身・人に投げる・投げる物"),
                 ex_("Stabilization, door stop, wedge", "安定させる・ドアストッパー・くさび"), ex_("Hiding, covering", "隠す・覆う"), ex_("Lining shelves", "棚に敷く")),
      `1` = list(ex_("Record keeping: birthdays, funerals, historical events", "記録として残す（誕生日・葬儀・歴史的な出来事）"), ex_("Wrap food", "食べ物を包む"),
                 ex_("Drive or bike over", "車や自転車でひく"), ex_("Make cushions / kneel on", "クッションにする・ひざをつく"), ex_("Wear as clothes", "服として着る"),
                 ex_("Crossword puzzles", "クロスワード"), ex_("Teach someone to read / read to others", "読み方を教える・読み聞かせ"),
                 ex_("Tent / teepee / human shelter", "テント・ティピー・シェルター"), ex_("Make an ad / sales", "広告を作る・販売"),
                 ex_("Stuff into shoes / hats to keep their shape", "靴や帽子に詰めて形を保つ")),
      `2` = list(ex_("Blocking wind below a door", "ドアの下のすき間風を防ぐ"), ex_("Food for termites", "シロアリのえさ"), ex_("Coffee filter", "コーヒーフィルター"),
                 ex_("Wet ball to shoot through a straw", "ぬらして丸め、ストローで飛ばす"), ex_("Keep the cat off the couch", "猫をソファに乗せないようにする"),
                 ex_("Cover fruit to ripen", "果物を包んで熟させる"), ex_("Wrap a wound", "傷を包む"),
                 ex_("Learn a new language from a foreign-language paper", "外国語の新聞で言葉を学ぶ"), ex_("Ballast for a hot-air balloon", "熱気球のおもり"),
                 ex_("Ear funnel to hear better", "耳に当てるじょうご（よく聞こえるように）"), ex_("Compare old and new papers to see how times change", "新旧の新聞を比べて時代の変化を見る"),
                 ex_("Donate to offices for reading", "オフィスに寄付して読んでもらう"), ex_("Pretend to read while watching someone", "読むふりをして様子をうかがう"),
                 ex_("Sit on it while coloring Easter eggs", "イースターエッグに色をぬるときの下敷き"), ex_("Silly Putty prints", "スライムで文字を写し取る"),
                 ex_("Use the ink to make fingerprints", "インクで指紋をとる"), ex_("Photo op", "写真の小道具"),
                 ex_("Find all the words in an article starting with p", "記事の中の p で始まる単語を探す"),
                 ex_("Compare children's heights / measure distance", "子どもの身長を比べる・距離を測る"))),
    invalid = "Schools, clinics, library, acting, look at, spell, grateful that I know how to read, feel, taste, put in water"),

  brick = list(
    en = "Brick", ja = "レンガ",
    aliases = c("brick", "bricks", "レンガ", "れんが", "煉瓦", "ブロック"),
    cats = list(
      cat_("Tool", "道具", c("道具", "ハンマー", "金づち", "釘", "くぎ", "研", "tool", "hammer", "nail")),
      cat_("Build (constructive)", "建てる（建設）", c("建", "家", "壁", "塀", "道", "階段", "花壇", "かまど", "暖炉", "炉", "囲", "build", "house", "wall", "path", "border")),
      cat_("Weapon", "武器", c("武器", "殴", "攻撃", "護身", "なぐ", "weapon", "hit", "attack")),
      cat_("Athletic (lifting, throwing)", "運動（持ち上げる・投げる）", c("筋トレ", "ダンベル", "トレーニング", "運動", "鍛", "投げ", "exercise", "lift", "throw", "workout")),
      cat_("Weight", "重し", c("重し", "おもし", "重り", "ドアストッパー", "押さえ", "文鎮", "沈め", "本立て", "weight", "doorstop", "door stop", "prop", "bookend")),
      cat_("Noise", "音", c("音", "楽器", "鳴ら", "noise", "sound", "music")),
      cat_("Stand / stacking (sitting)", "台・積む（座る）", c("台", "椅子", "いす", "座", "積", "踏み台", "乗", "stand", "sit", "stack", "step")),
      cat_("Artistic (decoration)", "アート（装飾）", c("飾", "装飾", "アート", "絵", "置物", "インテリア", "彫", "decor", "art", "paint")),
      cat_("Interactive", "遊び・やりとり", c("遊", "ゲーム", "おもちゃ", "play", "game", "toy")),
      cat_("Miscellaneous (scraping, etc.)", "その他（こする など）", c("こす", "削", "scrap", "scratch")),
      cat_("Measurement (counting, straight edge)", "測る（数える・定規）", c("定規", "測", "計", "数え", "measur", "count", "ruler")),
      cat_("Destructive (smashing, breaking)", "壊す（砕く・割る）", c("割", "壊", "砕", "粉", "つぶ", "smash", "break", "crush", "destroy"))),
    elab = list(ex_("“To throw”, “hit”, “build”, “bookend”, “weight”", "「投げる」「たたく」「建てる」「本立て」「重し」"),
                ex_("“To throw because you're feeling smashy”", "「むしゃくしゃしたときに投げる」"),
                ex_("“As a weight to keep the door shut from a big gust of wind”", "「強い風でドアが開かないように重しにする」")),
    orig = list(
      `0` = list(ex_("Building: houses, fire pits, roads", "建てる：家・たき火の炉・道"), ex_("Steps, stairs, pathway", "段・階段・小道"),
                 ex_("Border, fence, barrier, divider", "境界・塀・仕切り"), ex_("Weight: hold down, car block, door stop, anchor", "重し：押さえ・車止め・ドアストッパー・いかり"),
                 ex_("Exercise / yoga / martial arts", "運動・ヨガ・武道"), ex_("Smashing, crushing, breaking", "割る・つぶす・壊す"), ex_("Hammering", "金づち代わり"),
                 ex_("Weapon", "武器"), ex_("Stacking", "積む"), ex_("Art", "アート"), ex_("Pressing flowers, cheese, tofu", "押し花・チーズや豆腐の水切り"),
                 ex_("Measuring, drawing, straightedge", "測る・線を引く・定規"), ex_("Stand / sit on", "台にする・座る"),
                 ex_("Keep warm / heat transfer", "保温・熱を伝える"), ex_("Play, games, imagination, children", "遊び・ゲーム・子ども"),
                 ex_("Drainage for plants", "植木の水はけ"), ex_("Break up and use the powder", "砕いて粉を使う"), ex_("Writing a message on it", "メッセージを書く"),
                 ex_("Scraping, scratching, digging", "こする・ひっかく・掘る"), ex_("Pillow, bed, headrest", "枕・ベッド・頭置き")),
      `1` = list(ex_("Making a pattern", "模様を作る"), ex_("Give away / gift", "あげる・贈る"), ex_("Color comparison", "色の見本"),
                 ex_("Mold to make more bricks", "レンガの型にする"), ex_("Making marks, signalling where something is", "目印にする"), ex_("Meditate on", "瞑想に使う"),
                 ex_("Nail file", "爪やすり"), ex_("Block mice / raccoons / pests", "ネズミなどの害獣よけ"), ex_("Make noise", "音を出す"), ex_("Science experiments", "理科の実験")),
      `2` = list(ex_("Smooth / polish wood", "木を磨く"), ex_("Qi gong tool", "気功の道具"), ex_("Sink in water to raise the water level", "水に沈めて水位を上げる"),
                 ex_("Sharpen a knife", "包丁を研ぐ"), ex_("Eat food off it", "皿にする"), ex_("Habitat for a lizard", "トカゲのすみか"),
                 ex_("Engrave it to commemorate someone", "名前を彫って記念にする"), ex_("Teaching math", "算数を教える"), ex_("A washing surface", "洗濯板"),
                 ex_("Burning incense", "お香立て"), ex_("Cooling rack", "冷ましの台"))),
    invalid = "Ice skate, listening to the earth, observe"),

  envelope = list(
    en = "Envelope", ja = "封筒",
    aliases = c("envelope", "envelopes", "封筒", "ふうとう"),
    cats = list(
      cat_("Mailing / sending", "郵送・送る", c("送", "郵", "手紙", "出す", "届", "ポスト", "mail", "send", "post", "letter")),
      cat_("Containing (coin holder, locks of hair)", "入れる（小銭入れ・髪の毛など）", c("入れ", "保管", "しまう", "収納", "袋", "小銭", "種", "contain", "hold", "store", "coin")),
      cat_("Wrapping (including gum)", "包む（ガムなど）", c("包", "ガム", "くるむ", "wrap", "gum")),
      cat_("Crafts (origami, art, child play, toy)", "工作（折り紙・アート・子どもの遊び・おもちゃ）", c("折り紙", "折", "工作", "おもちゃ", "紙飛行機", "飛行機", "絵", "アート", "人形", "origami", "craft", "art", "toy", "airplane")),
      cat_("Stabilizer (under a short table leg)", "安定させる（短い机の脚の下に）", c("下に", "挟", "支え", "がたつき", "stabil", "under")),
      cat_("Miscellaneous (placemat, napkin, coaster, recycle)", "その他（敷物・ナプキン・コースター・リサイクル）", c("コースター", "ナプキン", "敷", "リサイクル", "coaster", "napkin", "placemat", "recycl")),
      cat_("Stationery (writing, notepad, filing, organizing)", "文具（書く・メモ帳・ファイル・整理）", c("書", "メモ", "整理", "ファイル", "リスト", "write", "note", "list", "fil", "organi")),
      cat_("Kindling (fire)", "焚き付け（火）", c("火", "燃", "着火", "焚", "fire", "burn", "kindling")),
      cat_("Tool (straight edge, crevice cleaner, bookmark, measuring, slicing)", "道具（定規・すき間の掃除・しおり・測る・切る）", c("しおり", "定規", "掃除", "すき間", "隙間", "切", "測", "すくう", "bookmark", "ruler", "clean", "cut", "scoop"))),
    elab = list(ex_("“Mail”, “bookmark”, “notepad”, “origami”", "「郵送」「しおり」「メモ帳」「折り紙」"),
                ex_("“Fold into a paper airplane”", "「折って紙飛行機にする」"),
                ex_("“Use to wrap up and throw away your gum”", "「ガムを包んで捨てる」")),
    orig = list(
      `0` = list(ex_("Mailing, sending", "郵送・送る"), ex_("Writing on", "書く"), ex_("Recycling, throwing away, compost", "リサイクル・捨てる・堆肥"),
                 ex_("Arts and crafts, drawing, painting, origami, toy", "工作・絵・折り紙・おもちゃ"), ex_("Holding objects, organizing, filing", "物を入れる・整理・ファイル"),
                 ex_("Hiding / revealing", "隠す・見せる"), ex_("Blocking light, sun, wind", "光・日差し・風をさえぎる"),
                 ex_("Cleaning, catching grease, picking up something you don't want to touch", "掃除・油を受ける・触りたくない物をつまむ"),
                 ex_("Measuring, straight line, ruler", "測る・直線・定規"), ex_("Burning, kindling", "燃やす・焚き付け"), ex_("Stabilization", "安定させる"),
                 ex_("Fan", "うちわ"), ex_("Carry / move objects and insects", "物や虫を運ぶ"), ex_("Kill insects", "虫をたたく"), ex_("Coaster, placemat", "コースター・敷物"),
                 ex_("Bookmark", "しおり"), ex_("Sign, label, name tag", "看板・ラベル・名札"), ex_("Throw it", "投げる"), ex_("Play, confetti", "遊び・紙吹雪")),
      `1` = list(ex_("Make noise with, whistle", "音を出す・笛"), ex_("Paper cut, slicing / cutting", "紙で切る"), ex_("Use the sticky part as glue", "のりの部分を接着に使う"),
                 ex_("Let an animal chew on it", "動物にかませる"), ex_("As an email icon", "メールのアイコン"), ex_("Toothpick", "つまようじ"),
                 ex_("Roll a cigarette", "たばこを巻く"), ex_("Chopstick rest", "箸置き")),
      `2` = list(ex_("Retrieve an object under a thin opening", "すき間の下の物を取り出す"), ex_("Sewing corners", "角を縫う型"), ex_("Over-dressing for a wound", "傷の上の当て紙"),
                 ex_("Divination", "占い"), ex_("Make a trail while hiking", "ハイキングの道しるべ"), ex_("Blot lipstick", "口紅を押さえる"), ex_("Mousepad", "マウスパッド"),
                 ex_("Keep a door from locking", "ドアがロックされないようにする"), ex_("Cup for drinking water", "水を飲むコップ"), ex_("Funnel", "じょうご"),
                 ex_("Testing nail-polish colors", "マニキュアの色見本"))),
    invalid = "Blow on as an instrument, a plate, a shoe for a small person, apply medicine with, paperclips, money, glue, pet, imaginary friend, mitten"),

  hanger = list(
    en = "Hanger", ja = "ハンガー",
    aliases = c("hanger", "coat hanger", "clothes hanger", "hangers", "ハンガー", "針金ハンガー", "衣紋掛け"),
    cats = list(
      cat_("Hanging", "掛ける", c("掛", "かけ", "干", "吊", "つる", "hang")),
      cat_("Reaching / tool", "届かせる・道具", c("届", "取る", "取り出", "引っ掛け", "鍵", "詰まり", "排水", "つつ", "reach", "retriev", "unlock", "unclog", "tool")),
      cat_("Holder (tie holder, watch holder)", "ホルダー（ネクタイ・時計を掛ける）", c("ネクタイ", "時計", "ホルダー", "置き", "スタンド", "holder", "tie", "watch")),
      cat_("Weapon", "武器", c("武器", "叩", "たたく", "刺", "攻撃", "weapon", "hit", "stab", "poke")),
      cat_("Art (wire art)", "アート（針金アート）", c("アート", "作品", "オブジェ", "リース", "飾", "art", "sculpt", "wreath", "decor")),
      cat_("Tying / binding", "しばる・つなぐ", c("縛", "しば", "結", "留め", "つな", "巻", "tying", "tie up", "bind", "connect")),
      cat_("Games (keep-away)", "ゲーム（遊び）", c("遊", "ゲーム", "輪投げ", "game", "play")),
      cat_("Electricity", "電気", c("アンテナ", "電気", "電線", "導", "antenna", "electric", "wire")),
      cat_("Miscellaneous (scrape)", "その他（こする）", c("こす", "削", "かく", "scrap", "scratch"))),
    elab = list(ex_("“Hanging clothes”, “remove hair from drain”, “to poke eyes”", "「服を掛ける」「排水口の髪を取る」"),
                ex_("“Making an angel shape out of it”", "「曲げて天使の形にする」"),
                ex_("“Making a brighter smile by putting it in the mouth”", "「口に入れて笑顔を大きく見せる」")),
    orig = list(
      `0` = list(ex_("Hang coats, clothes, ties, jewelry", "服・ネクタイ・アクセサリーを掛ける"), ex_("Unlocking a car / picking a lock", "車の鍵を開ける・鍵を開ける"),
                 ex_("Crafts", "工作"), ex_("TV antenna", "テレビのアンテナ"), ex_("Roasting marshmallows, cooking", "マシュマロを焼く・料理"),
                 ex_("Cleaning, unclogging a drain", "掃除・排水口の詰まりを取る"), ex_("Reaching", "届かない物を取る"), ex_("Back scratcher", "孫の手"),
                 ex_("Scratching surfaces, scraping paint, making marks", "こする・ペンキをはがす・印を付ける"), ex_("Hitting someone / weapon", "人をたたく・武器"),
                 ex_("Use as wire, conduct electricity", "針金として使う・電気を通す"), ex_("Binding, connecting, tying", "しばる・つなぐ"), ex_("Fence", "柵"),
                 ex_("Poking (including a fire stick)", "つつく（火かき棒など）"), ex_("Recycling, donating, trash", "リサイクル・寄付・捨てる"), ex_("Fishing pole", "釣りざお"),
                 ex_("Gardening, trellis, support plants", "園芸・支柱"), ex_("Bend into another shape", "曲げて別の形にする"),
                 ex_("Make jewelry, clothes, belt, hat, costume", "アクセサリー・服・ベルト・帽子・衣装を作る"), ex_("Straightedge, tracing, measuring", "定規・なぞる・測る"),
                 ex_("Games, play, sports (bow and arrow, hoop)", "ゲーム・遊び・スポーツ（弓矢・ゴール）"), ex_("Make a hole", "穴をあける"), ex_("Hanging other objects", "ほかの物を吊るす"),
                 ex_("Musical instrument, drumsticks", "楽器・ドラムスティック"), ex_("As a hook", "フックにする"), ex_("Animal trap", "動物のわな"), ex_("Flagpole", "旗ざお"),
                 ex_("Divining / dowsing rod", "ダウジングの棒"), ex_("Drying herbs / flowers", "ハーブや花を干す"), ex_("Melt and make something else", "溶かして別の物を作る")),
      `1` = list(ex_("Jaw / mouth opening", "口を開けておく"), ex_("Door or window stopper", "ドアや窓のストッパー"), ex_("Move hot things", "熱い物を動かす"),
                 ex_("Hang a photo on the wall", "写真を壁に掛ける"), ex_("Bubble blower", "シャボン玉の輪"), ex_("Rodent trap", "ネズミ捕り")),
      `2` = list(ex_("Make furniture", "家具を作る"), ex_("Stop sleep apnea by wearing it on the back", "背中に付けていびき・無呼吸を防ぐ"), ex_("Bookmark", "しおり"),
                 ex_("Relieve tension by twisting it", "ねじってストレス解消"), ex_("Make a magnetic coil or compass", "コイルや方位磁針を作る"), ex_("Tanning hide", "皮をなめす"),
                 ex_("Hold open a plastic bag", "ビニール袋の口を開けておく"), ex_("Posture reminder", "姿勢を正す道具"), ex_("Use as a splint", "添え木にする"))),
    invalid = "Hanging oneself, fix things")
)

AUT_RULES <- list(
  fluency = c("Fluency: count the acceptable uses. A use is unacceptable if it is not possible with this object.",
              "流暢性：受け入れられる用途の数。その物では不可能な用途は数えない。"),
  flexibility = c("Flexibility: the number of different categories. Building a house, a chimney and a stove are one category (building); building a house, throwing it at a person and a doorstop are three.",
                  "柔軟性：異なるカテゴリーの数。家・煙突・かまどを作るのは1つ（建てる）、家を建てる・人に投げる・ドアストッパーは3つ。"),
  elaboration = c("Elaboration: amount of detail, 0–2. “A doorstop” = 0; “a doorstop to prevent a door slamming shut in a strong wind” = 2 (one point for the explanation, one more for the detail about the wind).",
                  "精緻性：詳しさ（0〜2）。「ドアストッパー」= 0、「強い風でドアがバタンと閉まらないようにするドアストッパー」= 2（説明で1点、風についての詳しさでもう1点）。"),
  originality = c("Originality: compared with the other responses in the dataset, 0 = non-original, 1 = somewhat original, 2 = original. Someone with 10 common uses gets 0; someone with 3 uses, one of them unique, gets 2.",
                  "独創性：データ全体の他の回答と比べて、0＝独創的でない、1＝やや独創的、2＝独創的。ありふれた用途を10個書いた人は0、3個のうち1つが他にない用途なら2。")
)

# Match an item string to a codebook entry (NULL if none)
aut_item_key <- function(item) {
  x <- tolower(trimws(item))
  for (k in names(AUT_CODEBOOK)) if (x %in% tolower(AUT_CODEBOOK[[k]]$aliases)) return(k)
  NULL
}

# Keyword suggestions: category numbers ranked by number of keyword hits
aut_suggest <- function(response, cats) {
  if (!length(cats)) return(integer(0))
  x <- tolower(response)
  # full-width ASCII (Ａ, ｂ, １…) to half-width so English keywords also match
  cp <- utf8ToInt(enc2utf8(x))
  if (length(cp) && !anyNA(cp)) { fw <- cp >= 0xFF01 & cp <= 0xFF5E; cp[fw] <- cp[fw] - 0xFEE0; x <- tolower(intToUtf8(cp)) }
  hits <- vapply(cats, function(cc) {
    kw <- tolower(cc$kw %||% character(0)); kw <- kw[nzchar(kw)]
    if (!length(kw)) return(0)
    ascii <- !grepl("[^ -~]", kw)
    sum(vapply(seq_along(kw), function(i)
      if (ascii[i]) grepl(paste0("\\b", kw[i]), x, perl = TRUE) else grepl(kw[i], x, fixed = TRUE), TRUE))
  }, 0)
  ord <- order(-hits)
  ord[hits[ord] > 0]
}
