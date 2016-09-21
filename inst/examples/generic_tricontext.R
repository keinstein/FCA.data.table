library("FCA.data.table")
hostelmatrix <- as.logical(c(
#   hostelworld  hestels      hostelbookers
#   c y l s f c  c y l s f c  c y l s f c
    0,0,1,0,0,0, 0,0,1,1,0,0, 0,0,1,1,0,0, # Nuevo Suizo
    0,1,1,1,0,1, 0,1,1,1,0,1, 1,1,1,1,0,1, # Samay
    1,1,1,1,0,1, 1,1,1,1,1,1, 0,1,1,1,1,1, # Oasis Backpacker
    1,1,0,1,0,1, 1,1,1,1,1,1, 1,1,1,1,1,1, # One
    1,1,0,1,0,1, 1,1,1,1,1,1, 1,1,1,1,1,1, # Ole Backpacker
    0,0,1,1,0,1, 1,1,1,1,1,1, 1,1,1,1,0,1  # Garden Backpacker
))

hm1 <- expand.grid(
    attribute = c("character","safety","location",
        "staff","fun","cleanliness"),
    condition = c("hostelworld","hostels","hostelbookers"),
    object = c("Nuevo Suizo","Samay","Oasis Backpacker",
        "One","Ole Backpacker","Garden Backpacker"),
    stringsAsFactors=FALSE)

hostels <- tricontext.data.table(hm1[hostelmatrix,],
                                 objects = unique(hm1[,"object"]),
                                 attributes = unique(hm1[,"attribute"]),
                                 conditions = unique(hm1[,"condition"])
                                 )

he <- extent(hostels,c("Nuevo Suizo",
                       "Samay",
                       "Oasis Backpacker",
                       "Garden Backpacker"))
hi <- intent(hostels,c("location"))
hm <- modus(hostels,c("hostelbookers"))

modintent(he)
# Triadic modus×intent: 
#        condition
# 1:   hostelworld
# 2:       hostels
# 3: hostelbookers
#    attribute
# 1:  location
# 2:     staff

intent(he,hm)
# Triadic intent: 
#    attribute
# 1:  location
# 2:     staff

concept(he,hi)
