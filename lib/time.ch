import

"lib/strings.ch"::"strs"

gocode "time"
gocode "errors"

def

num = 42

Duration = struct(hours, minutes, seconds, nanoseconds int)

Weekday = enum SUN, MON, TUE, WED, THUR, FRI, SAT

weekday(t Time) :
    Weekday[goGetWeekday(t[year], t[month], t[day], t[hour], t[min], t[loc])]

goGetWeekday(year, month, day, hour, min int, loc string) : gocode {
    locObj, err := time.LoadLocation(loc)
    if err != nil {
        return err
    }
    var goMonth time.Month
    switch month {
        case 1 :
            goMonth = time.January
        case 2 :
            goMonth = time.February
        case 3 :
            goMonth = time.March
        case 4 :
            goMonth = time.April
        case 5 :
            goMonth = time.May
        case 6 :
            goMonth = time.June
        case 7 :
            goMonth = time.July
        case 8 :
            goMonth = time.August
        case 9 :
            goMonth = time.September
        case 10 :
            goMonth = time.October
        case 11 :
            goMonth = time.November
        case 12 :
            goMonth = time.December
        default :
            return errors.New("can't convert month")   
    }
    return int((time.Date(year, goMonth, day, hour, min, 0, 0, locObj)).Weekday())
}

// func (t Time) Add(d Duration) Time
// func (t Time) AddDate(years int, months int, days int) Time
// func (t Time) After(u Time) bool
// func (t Time) AppendFormat(b []byte, layout string) []byte
// func (t Time) Before(u Time) bool
// func (t Time) Clock() (hour, min, sec int)
// func (t Time) Compare(u Time) int
// func (t Time) Date() (year int, month Month, day int)
// func (t Time) Day() int
// func (t Time) Equal(u Time) bool
// func (t Time) Format(layout string) string
// func (t Time) GoString() string
// func (t *Time) GobDecode(data []byte) error
// func (t Time) GobEncode() ([]byte, error)
// func (t Time) Hour() int
// func (t Time) ISOWeek() (year, week int)
// func (t Time) In(loc *Location) Time
// func (t Time) IsDST() bool
// func (t Time) IsZero() bool
// func (t Time) Local() Time
// func (t Time) Location() *Location
// func (t Time) MarshalBinary() ([]byte, error)
// func (t Time) MarshalJSON() ([]byte, error)
// func (t Time) MarshalText() ([]byte, error)
// func (t Time) Minute() int
// func (t Time) Month() Month
// func (t Time) Nanosecond() int
// func (t Time) Round(d Duration) Time
// func (t Time) Second() int
// func (t Time) String() string
// func (t Time) Sub(u Time) Duration
// func (t Time) Truncate(d Duration) Time
// func (t Time) UTC() Time
// func (t Time) Unix() int64
// func (t Time) UnixMicro() int64
// func (t Time) UnixMilli() int64
// func (t Time) UnixNano() int64
// func (t *Time) UnmarshalBinary(data []byte) error
// func (t *Time) UnmarshalJSON(data []byte) error
// func (t *Time) UnmarshalText(data []byte) error
// func (t Time) Weekday() Weekday
// func (t Time) Year() int
// func (t Time) YearDay() int
// func (t Time) Zone() (name string, offset int)
// func (t Time) ZoneBounds() (start, end Time)
