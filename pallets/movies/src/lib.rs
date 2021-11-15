#![cfg_attr(not(feature = "std"), no_std)]

pub use pallet::*;

// #[cfg(test)]
// mod mock;

// #[cfg(test)]
// mod tests;

// #[cfg(feature = "runtime-benchmarks")]
// mod benchmarking;

#[frame_support::pallet]
pub mod pallet {
    use frame_support::{
        pallet_prelude::*,
        sp_std::prelude::*,
        sp_runtime::traits::Hash,
        traits::{tokens::ExistenceRequirement, Currency, Randomness},
        transactional,
    };
    use frame_system::pallet_prelude::*;
    use scale_info::TypeInfo;
    // use sp_io::hashing::blake2_128;

    #[cfg(feature = "std")]
    use frame_support::serde::{Deserialize, Serialize};

    type AccountOf<T> = <T as frame_system::Config>::AccountId;
    type BalanceOf<T> =
        <<T as Config>::Currency as Currency<<T as frame_system::Config>::AccountId>>::Balance;

    #[derive(Clone, Encode, Decode, PartialEq, RuntimeDebug, TypeInfo)]
    #[scale_info(skip_type_params(T))]
    pub struct Movie<T: Config> {
        pub name: [u8; 16],
        pub producer: AccountOf<T>,
        pub rent_cost: Option<BalanceOf<T>>,
        pub rated: MovieRated,
    }

    #[derive(Clone, Encode, Decode, PartialEq, RuntimeDebug, TypeInfo)]
    #[scale_info(skip_type_params(T))]
    pub struct Review<T: Config> {
        pub rating: u8,
        pub text: Option<[u8; 16]>,
        pub movie: T::Hash,
    }

    #[derive(Clone, Encode, Decode, PartialEq, RuntimeDebug, TypeInfo)]
    #[scale_info(skip_type_params(T))]
    #[cfg_attr(feature = "std", derive(Serialize, Deserialize))]
    pub enum MovieRated {
        Kids,
        Family,
        PG,
    }

    #[pallet::pallet]
    #[pallet::generate_store(pub(super) trait Store)]
    // #[pallet::generate_storage_info]
    pub struct Pallet<T>(_);

    #[pallet::config]
    pub trait Config: frame_system::Config {
        /// Because this pallet emits events, it depends on the runtime's definition of an event.
        type Event: From<Event<Self>> + IsType<<Self as frame_system::Config>::Event>;

        /// The Currency handler for the Kitties pallet.
        type Currency: Currency<Self::AccountId>;

        /// The maximum amount of Movies a single account can rent.
        #[pallet::constant]
        type MaxMoviesRented: Get<u32>;

        /// The type of Randomness we want to specify for this pallet.
        type MovieRatedRandomness: Randomness<Self::Hash, Self::BlockNumber>;
    }

    #[pallet::error]
    pub enum Error<T> {
        /// Handles arithemtic overflow when incrementing the Kitty counter.
        MoviesCntOverflow,
        /// An account cannot rent more Movies than `MaxMoviesRented`.
        ExceedMaxMoviesRented,
        /// Renter cannot be the owner.
        RenterIsMovieProducer,
        /// Handles checking whether the Kitty exists.
        MovieNotExist,
        /// Handles checking that the Movie is owned by the account.
        NotMovieProducer,
        /// Ensures the Movie is not already rented by the person.
        MovieAlreadyRented,
        /// Ensures the Movie is available to rent.
        MovieNotForRent,
        /// Ensures that the reviewer has rented the Movie in the past.
        ReviewUnRentedMovie,
        /// Ensures that the reviewer has not already reviewed the Movie in the past.
        AlreadyReviewedMovie,
        /// Handles checking whether the Review exists..
        ReviewNotExist,
        /// Ensures that the renter price is greater than the asking price.
        MovieRentPriceTooLow,
        /// Ensures that the renter price is greater than the asking price.
        RatingOutOfBounds,
        /// Ensures that an account has enough funds to rent a Movie.
        NotEnoughBalance,
    }

    #[pallet::event]
    #[pallet::generate_deposit(pub(super) fn deposit_event)]
    pub enum Event<T: Config> {
        /// A new Movie was sucessfully produced. \[sender, movie_id\]
        Produced(T::AccountId, T::Hash),
        /// Movie price was sucessfully set. \[sender, movie_id, new_price\]
        RentCostSet(T::AccountId, T::Hash, Option<BalanceOf<T>>),
        /// A Movie was sucessfully rented. \[producer, to, movie_id, bid_price\]
        Rented(T::AccountId, T::Hash, BalanceOf<T>),
        /// A Movie was successfully given a review. \[renter, movie_id\]
        Reviewed(T::AccountId, T::Hash),
    }

    #[pallet::storage]
    #[pallet::getter(fn movies_cnt)]
    /// Keeps track of the number of Movies in existence.
    pub(super) type MoviesCnt<T: Config> = StorageValue<_, u64, ValueQuery>;

    #[pallet::storage]
    #[pallet::getter(fn movies_rented)]
    /// Keeps track of what accounts have rented what Movies.
    pub(super) type MoviesRented<T: Config> = StorageMap<
        _,
        Twox64Concat,
        T::AccountId,
        BoundedVec<T::Hash, T::MaxMoviesRented>,
        ValueQuery,
    >;

    #[pallet::storage]
    #[pallet::getter(fn authors_reviews)]
    /// Keeps track of what movie has what reviews.
    pub(super) type MoviesReviewed<T: Config> = StorageMap<
        _,
        Twox64Concat,
        T::AccountId,
        Vec<T::Hash>,
        ValueQuery,
    >;

    #[pallet::storage]
    #[pallet::getter(fn movies)]
    /// Stores a Movie's unique traits, name, reviews, owner and rent cost.
    pub(super) type Movies<T: Config> = StorageMap<_, Twox64Concat, T::Hash, Movie<T>>;

    #[pallet::storage]
    #[pallet::getter(fn reviews)]
    /// Stores a Review's unique traits, rating, text and Movie hash.
    pub(super) type Reviews<T: Config> = StorageMap<_, Twox64Concat, T::Hash, Review<T>>;

    #[pallet::call]
    impl<T: Config> Pallet<T> {
        #[pallet::weight(100)]
        pub fn create_movie(origin: OriginFor<T>, name: [u8; 16]) -> DispatchResult {
            let sender = ensure_signed(origin)?;

            let movie_id = Self::mint(&sender, name, None)?;
            log::info!("A movie is produced with ID: {:?} by the producer ID: {:?}.", movie_id, sender);
            Self::deposit_event(Event::Produced(sender, movie_id));

            Ok(())
        }

        #[pallet::weight(100)]
        pub fn set_rent_cost(origin: OriginFor<T>, movie_id: T::Hash, new_price: Option<BalanceOf<T>>) -> DispatchResult {
            let sender = ensure_signed(origin)?;
            // Ensure the movie exists and is called by the movie producer
            ensure!(Self::is_movie_producer(&movie_id, &sender)?, <Error<T>>::NotMovieProducer);

            let mut movie = Self::movies(&movie_id).ok_or(<Error<T>>::MovieNotExist)?;
            movie.rent_cost = new_price.clone();
            <Movies<T>>::insert(&movie_id, movie);
            Self::deposit_event(Event::RentCostSet(sender, movie_id, new_price));

            Ok(())
        }

        #[pallet::weight(100)]
        pub fn rent(origin: OriginFor<T>, movie_id: T::Hash, bid_price: BalanceOf<T>) -> DispatchResult {
            let to = ensure_signed(origin)?;

            // Ensure the movie exists and renter is not the producer.
            ensure!(!Self::is_movie_producer(&movie_id, &to)?, <Error<T>>::RenterIsMovieProducer);
            // Ensure Movie is available to rent, and the bid_price is more than the rent price.
            let movie = Self::movies(&movie_id).ok_or(<Error<T>>::MovieNotExist)?;
            if let Some(rent_cost) = movie.rent_cost {
                ensure!(rent_cost <= bid_price, <Error<T>>::MovieRentPriceTooLow)
            } else {
                Err(<Error<T>>::MovieNotForRent)?;
            }
            // Check the buyer has enough free balance
            ensure!(T::Currency::free_balance(&to) >= bid_price, <Error<T>>::NotEnoughBalance);

            Self::rent_movie_to(&movie_id, &to)?;
            Self::deposit_event(Event::Rented(to, movie_id, bid_price));

            Ok(())
        }

        #[pallet::weight(100)]
        pub fn review(origin: OriginFor<T>, movie_id: T::Hash, rating: u8, text: Option<[u8; 16]>) -> DispatchResult {
            let from = ensure_signed(origin)?;

            Self::review_movie(&movie_id, &from, rating, text)?;
            Self::deposit_event(Event::Reviewed(from, movie_id));

            Ok(())
        }
    }

    impl<T: Config> Pallet<T> {
        pub fn is_movie_producer(movie_id: &T::Hash, acct: &T::AccountId) -> Result<bool, Error<T>> {
            match Self::movies(movie_id) {
                Some(movie) => Ok(movie.producer == *acct),
                None => Err(<Error<T>>::MovieNotExist)
            }
        }

        #[transactional]
        pub fn mint(producer: &T::AccountId, movie_name: [u8; 16], movie_rated: Option<MovieRated>) -> Result<T::Hash, Error<T>> {
            let movie = Movie::<T> {
                name: movie_name,
                producer: producer.clone(),
                rent_cost: None,
                rated: movie_rated.unwrap_or_else(Self::gen_movie_rated),
            };

            let movie_id = T::Hashing::hash_of(&movie);
            // Performs this operation first as it may fail
            let new_cnt = Self::movies_cnt().checked_add(1).ok_or(<Error<T>>::MoviesCntOverflow)?;

            <Movies<T>>::insert(movie_id, movie);
            <MoviesCnt<T>>::put(new_cnt);
            Ok(movie_id)
        }

        fn gen_movie_rated() -> MovieRated {
            let random = T::MovieRatedRandomness::random(&b"rated"[..]).0;
            match random.as_ref()[0] % 3 {
                0 => MovieRated::Kids,
                1 => MovieRated::Family,
                _ => MovieRated::PG,
            }
        }

        pub fn movie_has_been_rented(movie_id: &T::Hash, to: &T:: AccountId) -> Result<bool, Error<T>> {
            ensure!(Self::movies(movie_id).is_some(), <Error<T>>::MovieNotExist);
            <MoviesRented<T>>::try_mutate(to, |rented| {
                if let Some(_) = rented.iter().position(|&id| id == *movie_id) {
                    return Ok(true);
                }
                Ok(false)
            })
        }

        pub fn movie_has_been_reviewed(movie_id: &T::Hash, from: &T:: AccountId) -> Result<bool, Error<T>> {
            ensure!(Self::movies(movie_id).is_some(), <Error<T>>::MovieNotExist);
            <MoviesReviewed<T>>::try_mutate(from, |review_ids| {
                for review_id in review_ids.iter() {
                    let review = <Reviews<T>>::get(review_id).ok_or(<Error<T>>::ReviewNotExist)?;
                    if review.movie == *movie_id {
                        return Ok(true);
                    }
                }
                Ok(false)
            })
        }

        #[transactional]
        pub fn rent_movie_to(movie_id: &T::Hash, to: &T::AccountId) -> Result<(), Error<T>> {
            // Ensure movie has a rent cost set.
            ensure!(
                Self::movies(movie_id).ok_or(<Error<T>>::MovieNotExist)?.rent_cost.is_some(),
                <Error<T>>::MovieNotForRent
            );
            // Ensure that the Movie has not already been rented by the account.
            ensure!(!Self::movie_has_been_rented(movie_id, to)?, <Error<T>>::MovieAlreadyRented);
            // Make sure the account can rent another movie.
            <MoviesRented<T>>::try_mutate(to, |movie_vec| movie_vec.try_push(*movie_id))
                .map_err(|_| <Error<T>>::ExceedMaxMoviesRented)?;
            Ok(())
        }

        #[transactional]
        pub fn review_movie(movie_id: &T::Hash, from: &T::AccountId, rating: u8, text: Option<[u8; 16]>) -> Result<(), Error<T>> {
            // Ensure the provided rating is within the range.
            ensure!(rating <= 10, <Error<T>>::RatingOutOfBounds);
            // Ensure the reviewer has rented the movie in the past.
            ensure!(Self::movie_has_been_rented(movie_id, from)?, <Error<T>>::ReviewUnRentedMovie);
            // Ensure the reviewer has not reviewed this movie in the past.
            ensure!(!Self::movie_has_been_reviewed(movie_id, from)?, <Error<T>>::AlreadyReviewedMovie);
            let review = Review::<T> {
                rating: rating,
                text: text,
                movie: movie_id.clone(),
            };
            let review_id = T::Hashing::hash_of(&review);
            <MoviesReviewed<T>>::mutate(from, |review_vec| review_vec.push(review_id));
            <Reviews<T>>::insert(review_id, review);
            Ok(())
        }
    }
}
